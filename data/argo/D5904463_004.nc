CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:47Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221447  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_004                   2C  D   APEX                            6530                            072314                          846 @�.�&?�1   @�/DD @)� ě���cq�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B)33B/��B6ffB@  BH  BP��BW��B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy�fD� D�@ D���D��3D�fD�9�D�l�D�ɚD�  D�VfD�vfD�ɚD�	�D�P D�vfD��3D��D�L�D�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�(�A{A&{AF{Af{A�
=A�
=A�
=A��
A�
=A�
=A�
=A�
=B�B	�B�B�B!�B*�RB1�B7�BA�BI�BRQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B���B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B���B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*z�C,z�C.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDt�Dy޸D�)D�L)D���D��\D��D�E�D�x�D���D�)D�b�D���D���D��D�\)Dڂ�D��\D�%�D�X�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�1'A�33A�33A�33A�7LA�5?A�1'A�7LA�;dA�=qA�;dA�=qA�=qA�?}A�?}A�9XA�(�A�-A�+Aڣ�A��A�\)A�O�A�1AžwA�XA��#A��jA���A��A��A�VA�+A�1A��/A�~�A��A�A���A�hsA�/A���A�=qA��FA��RA�(�A��A��A��A�S�A�33A��-A�ƨA��A���A�ȴA�33A�&�A�C�A�(�A�A�~�A�z�A��7A���Awx�Ao�hAj�AgXAbQ�A_�-A]K�AYS�AW�#AV1AUAS"�AM;dAKO�AF��A@I�A;\)A7��A7/A7�PA7?}A6�9A6bNA5ƨA5`BA5VA5x�A6A�A6jA6VA4��A2�/A0�HA0�RA0��A0v�A0r�A0z�A0v�A/�A.��A.ĜA-�^A+oA)�#A)�7A)��A)+A'��A'�A&��A&�RA&��A&r�A&�A&�A%�A$�/A$ �A#�FA"bA!`BA!%A!;dA -At�A�AZA�AdZA/A�A�A��A��A�A(�A=qA$�A��A�7A`BAdZAC�A�/A~�A�jA��AAO�A�A+A��A�A  A$�A�7A��Az�AVA=qAn�A��AA�A�A�A1'A�#AhsA
=A��AI�A��A��AA��Az�AA�A��A��AK�AO�A/A�HA�HA�`A�-A$�A�AA�#A�PA�AĜA�+A^5AM�AA�A�A�;A�
A�7A?}A�AjA�Ax�AO�A/A
=A
A�A	�^A	�A	\)A	K�A	/A	VA��A�HA�!AQ�A��AȴA�RA�\A��A�PAXAoA��AffA{A�TA�^At�A\)AG�A/A��A�A�!A�9A~�AM�AJA�-A�AG�A �/A -@�C�@�^5@���@��`@�b@�l�@���@�7L@��j@�S�@�ff@�5?@���@��@��T@��#@���@�Ĝ@�F@�+@�!@�J@�/@��@��@�-@�@�x�@�V@�  @�@�P@���@��@�Z@�@�M�@�%@䛦@�I�@�@�^5@��@�7@�&�@�j@�l�@��@ޗ�@�=q@���@ݡ�@�7L@ܬ@�  @۝�@�
=@�@�7L@�Ĝ@�1'@�+@֟�@�@Ձ@�O�@�?}@�&�@��@�%@Դ9@�9X@��;@�
=@��@�V@д9@���@Χ�@�@�?}@�Ĝ@��;@˅@��y@ʏ\@�ff@ɺ^@��/@ȃ@�bN@�9X@ǶF@�C�@ư!@�V@Ų-@�&�@ēu@�9X@�1@�l�@�~�@�$�@��@�@���@��@��
@�;d@��!@�^5@�=q@�x�@��`@��u@�A�@�  @���@�C�@���@��!@��@�p�@�?}@���@�b@��
@��@��P@�K�@�+@�
=@��y@�-@�hs@�Ĝ@���@�(�@�t�@��@��!@�v�@�$�@���@��@�X@��@��@�r�@�  @���@��@�n�@�=q@�$�@��T@��j@�r�@�1@��
@�|�@�33@��R@�ff@�$�@��@��-@�hs@�%@���@��@� �@�ƨ@��@��@�ff@�=q@�@��h@�7L@���@�bN@��;@�t�@�dZ@�S�@���@�E�@��T@��-@��7@�?}@��@�I�@��F@�|�@��@��H@�M�@�$�@��@��^@���@�x�@�&�@���@��@��@���@�S�@��@���@�ff@�E�@�-@��@���@�O�@�7L@���@��@�Q�@� �@�  @��@�l�@�;d@�
=@���@�5?@��
@��T@��;@���@xbN@p��@g�@_��@X1'@Q�7@H�@?�w@8  @2��@+ƨ@%�-@��@��@��@�@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�+A�1'A�33A�33A�33A�7LA�5?A�1'A�7LA�;dA�=qA�;dA�=qA�=qA�?}A�?}A�9XA�(�A�-A�+Aڣ�A��A�\)A�O�A�1AžwA�XA��#A��jA���A��A��A�VA�+A�1A��/A�~�A��A�A���A�hsA�/A���A�=qA��FA��RA�(�A��A��A��A�S�A�33A��-A�ƨA��A���A�ȴA�33A�&�A�C�A�(�A�A�~�A�z�A��7A���Awx�Ao�hAj�AgXAbQ�A_�-A]K�AYS�AW�#AV1AUAS"�AM;dAKO�AF��A@I�A;\)A7��A7/A7�PA7?}A6�9A6bNA5ƨA5`BA5VA5x�A6A�A6jA6VA4��A2�/A0�HA0�RA0��A0v�A0r�A0z�A0v�A/�A.��A.ĜA-�^A+oA)�#A)�7A)��A)+A'��A'�A&��A&�RA&��A&r�A&�A&�A%�A$�/A$ �A#�FA"bA!`BA!%A!;dA -At�A�AZA�AdZA/A�A�A��A��A�A(�A=qA$�A��A�7A`BAdZAC�A�/A~�A�jA��AAO�A�A+A��A�A  A$�A�7A��Az�AVA=qAn�A��AA�A�A�A1'A�#AhsA
=A��AI�A��A��AA��Az�AA�A��A��AK�AO�A/A�HA�HA�`A�-A$�A�AA�#A�PA�AĜA�+A^5AM�AA�A�A�;A�
A�7A?}A�AjA�Ax�AO�A/A
=A
A�A	�^A	�A	\)A	K�A	/A	VA��A�HA�!AQ�A��AȴA�RA�\A��A�PAXAoA��AffA{A�TA�^At�A\)AG�A/A��A�A�!A�9A~�AM�AJA�-A�AG�A �/A -@�C�@�^5@���@��`@�b@�l�@���@�7L@��j@�S�@�ff@�5?@���@��@��T@��#@���@�Ĝ@�F@�+@�!@�J@�/@��@��@�-@�@�x�@�V@�  @�@�P@���@��@�Z@�@�M�@�%@䛦@�I�@�@�^5@��@�7@�&�@�j@�l�@��@ޗ�@�=q@���@ݡ�@�7L@ܬ@�  @۝�@�
=@�@�7L@�Ĝ@�1'@�+@֟�@�@Ձ@�O�@�?}@�&�@��@�%@Դ9@�9X@��;@�
=@��@�V@д9@���@Χ�@�@�?}@�Ĝ@��;@˅@��y@ʏ\@�ff@ɺ^@��/@ȃ@�bN@�9X@ǶF@�C�@ư!@�V@Ų-@�&�@ēu@�9X@�1@�l�@�~�@�$�@��@�@���@��@��
@�;d@��!@�^5@�=q@�x�@��`@��u@�A�@�  @���@�C�@���@��!@��@�p�@�?}@���@�b@��
@��@��P@�K�@�+@�
=@��y@�-@�hs@�Ĝ@���@�(�@�t�@��@��!@�v�@�$�@���@��@�X@��@��@�r�@�  @���@��@�n�@�=q@�$�@��T@��j@�r�@�1@��
@�|�@�33@��R@�ff@�$�@��@��-@�hs@�%@���@��@� �@�ƨ@��@��@�ff@�=q@�@��h@�7L@���@�bN@��;@�t�@�dZ@�S�@���@�E�@��T@��-@��7@�?}@��@�I�@��F@�|�@��@��H@�M�@�$�@��@��^@���@�x�@�&�@���@��@��@���@�S�@��@���@�ff@�E�@�-@��@���@�O�@�7L@���@��@�Q�@� �@�  @��@�l�@�;d@�
=@���G�O�@��
@��T@��;@���@xbN@p��@g�@_��@X1'@Q�7@H�@?�w@8  @2��@+ƨ@%�-@��@��@��@�@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ǮB�B��B�B��B�BB$�B�B<jB�B��B��B��B��B�'B�B��B��B�VB}�BZB49B�BbB��B��BB%B'�B%�BVB)�BM�B:^B�B��BɺB��B^5B
�B
��B
m�B
L�B
-B	�B	��B	e`B	D�B	33B	%�B	&�B	%�B	-B	5?B	:^B	F�B	\)B	aHB	aHB	O�B	&�B	�B	.B	:^B	K�B	e`B	z�B	��B	�!B	�3B	��B	�B
{B
�B
)�B
,B
 �B
�B
0!B
:^B
J�B
M�B
W
B
ZB
YB
Q�B
O�B
M�B
<jB
2-B
/B
49B
7LB
8RB
;dB
>wB
@�B
D�B
H�B
I�B
P�B
Q�B
VB
P�B
L�B
D�B
A�B
B�B
K�B
J�B
H�B
C�B
F�B
M�B
J�B
M�B
S�B
S�B
S�B
R�B
Q�B
P�B
VB
VB
T�B
ZB
\)B
_;B
_;B
]/B
aHB
gmB
hsB
dZB
`BB
_;B
ffB
e`B
^5B
`BB
bNB
_;B
ZB
[#B
[#B
[#B
`BB
e`B
bNB
XB
R�B
O�B
M�B
N�B
O�B
Q�B
Q�B
R�B
Q�B
P�B
O�B
O�B
O�B
O�B
O�B
Q�B
T�B
YB
[#B
\)B
_;B
o�B
w�B
w�B
w�B
v�B
u�B
t�B
s�B
r�B
q�B
q�B
p�B
p�B
o�B
o�B
n�B
l�B
jB
hsB
ffB
cTB
dZB
cTB
aHB
]/B
[#B
YB
XB
W
B
VB
T�B
S�B
S�B
Q�B
O�B
J�B
I�B
I�B
H�B
G�B
F�B
E�B
D�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
B�B
B�B
B�B
B�B
D�B
F�B
F�B
E�B
D�B
D�B
C�B
B�B
@�B
>wB
;dB
9XB
8RB
8RB
7LB
5?B
5?B
49B
2-B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
0!B
0!B
/B
-B
,B
,B
,B
,B
+B
)�B
(�B
'�B
(�B
&�B
$�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
oB
\B
PB
JB
JB
JB
JB
DB
DB
DB

=B

=B
DB
DB

=B

=B
DB
DB
DB

=B

=B
DB
DB
DB

=B
	7B

=B

=B

=B

=B

=B
	7B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
PB
VB
VB
VB
\B
\B
\B
\B
\B
VB
\B
\B
\B
\B
\B
bB
bB
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
hB
hB
uB
oB
uB
oB
uB
oB
oB
uB
uB
uB
uB
uB
{B
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
)�B
0!B
7LB
=qB
B�B
G�B
L�B
P�B
T�B
YB
_;B
cTB
gmB
k�B
o�B
t�B
w�B
z�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ǑBkB��B��B��B�B�B$�B�B<HB�B��B��B��B��B�B��B��B��B�6B}�BY�B4B�B>B��B��B�BB'�B%�B/B)�BM�B:6BhB��BɖB��B^B
�[B
��B
mpB
L�B
,�B	�B	��B	eEB	D�B	3B	%�B	&�B	%�B	,�B	5#B	:DB	F�B	\B	a,B	a+B	O�B	&�B	�B	-�B	:BB	K�B	eAB	z�B	��B	��B	�B	ͳB	�B
XB
vB
)�B
+�B
 �B
�B
/�B
:6B
J�B
M�B
V�B
Y�B
X�B
Q�B
O�B
M�B
<CB
2	B
.�B
4B
7%B
8*B
;>B
>QB
@[B
DvB
H�B
I�B
P�B
Q�B
U�B
P�B
L�B
DsB
AcB
BiB
K�B
J�B
H�B
CmB
F�B
M�B
J�B
M�B
S�B
S�B
S�B
R�B
Q�B
P�B
U�B
U�B
T�B
Y�B
\B
_B
_B
]B
aB
gCB
hIB
d0B
`B
_B
f<B
e7B
^B
`B
b%B
_B
Y�B
Z�B
Z�B
Z�B
`B
e8B
b$B
W�B
R�B
O�B
M�B
N�B
O�B
Q�B
Q�B
R�B
Q�B
P�B
O�B
O�B
O�B
O�B
O�B
Q�B
T�B
X�B
Z�B
\ B
_B
otB
w�B
w�B
w�B
v�B
u�B
t�B
s�B
r�B
qB
qB
pyB
pyB
ouB
ouB
nqB
lbB
jVB
hGB
f;B
c*B
d/B
c)B
aB
]B
Z�B
X�B
W�B
V�B
U�B
T�B
S�B
S�B
Q�B
O�B
J�B
I�B
I�B
H�B
G�B
FB
ExB
DtB
CmB
CoB
CoB
CmB
BhB
BhB
AaB
BgB
BgB
BgB
BeB
DtB
F~B
F}B
EzB
DsB
DqB
ClB
BfB
@[B
>NB
;;B
9/B
8*B
8)B
7$B
5B
5B
4B
2B
0�B
0�B
2B
2B
2B
2B
2B
2B
0�B
0�B
/�B
/�B
.�B
,�B
+�B
+�B
+�B
+�B
*�B
)�B
(�B
'�B
(�B
&�B
$�B
"�B
 �B
�B
|B
xB
pB
oB
wB
vB
rB
qB
kB
cB
eB
kB
qB
rB
qB
kB
jB
iB
dB
_B
ZB
WB
VB
PB
RB
PB
QB
WB
TB
XB
]B
]B
^B
]B
VB
RB
RB
MB
KB
EB
4B
&B
B
 B
 B
!B
B
B
B

B

B
B
B

B

B
B
B
B

B

B
B
B
B

B
	B

B

B

B

B

B
	B
	B
	B

B
B
B
B
B
 B
!B
&B
'B
'B
%B
&B
&B
+B
'B
.B
+B
+B
/B
2B
3B
4B
1B
*B
2B
3B
1B
4B
3B
7B
8B
2B
0B
7B
?B
=B
=B
<B
=B
>B
<B
=B
=B
CB
EB
<B
=B
JB
BB
KB
CB
JB
EB
BB
IB
JB
IB
JB
JB
OB
KB
KB
JB
PB
OB
QB
iB
cB
iB
iB
gB
hB
jB
bB
iB
iB
bB
iB
qB
oB
tB
uB
pB
qB
sB
qB
pB
wB
oB
sB
tB
uB
sB
vB
xB
zB
}B
|B
|B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
"�B
)�B
/�B
7B
=DB
BdB
G�B
L�B
P�B
T�B
X�B
_B
c(B
gAB
kVB
osB
t�B
w�B
z�B
}�B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221447    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221447  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221447  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                