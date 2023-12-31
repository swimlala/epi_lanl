CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-17T19:16:25Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150517191625  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               sA   AO  4055_7112_115                   2C  D   APEX                            5374                            041511                          846 @�Q``��1   @�Q`���@<F$�/��d=����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    sA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D�3D�S3D�|�D���D�3D�C3D�33D��fD���D�I�D�y�D���D��D�L�D�y�D�� D���D�C3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCz�C aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDG�CFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD ��D!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt�RDy��D�\D�_\D���D���D�\D�O\D�?\D�ҏD��D�U�D���D���D�%�D�X�Dڅ�D��)D��D�O\D�|)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�33A�33A�33A�5?A�1'A���A���A�jA�"�A���A��mA���A���A��A�r�A�dZA��A���A��A�9XA�{A�I�A�?}A�(�A���A�ƨA��PA�I�A�S�A�^5A�$�A�{A���A���A��wA���A�ffA��A��A���A�VA���A���A��A���A���A�JA��A�{A�ZA��`A��A�
=A�ZA��;A�|�A�hsA�Q�A�x�A�33A��+A��#A��A�ffA��mA���A���A��A��9A�t�A�&�A��`A���A�1'A�
=A���A��yA��#A��FA��uA�=qA��wA�1'A�M�A��A�ĜA���A�Q�A��`A�v�A�1A��^A�(�A��`A�x�A�l�A�O�A�7LA�"�A��A�A�A�A���A��A���A�A�C�A���A�ZA�z�A���A�p�A���A�S�A��#A��hA���A��+A��DA���A�l�A��A~�A~��A}��A|��Az-Ayt�Ax  AvbNAu��As��Ar�Aq�Ap=qAoAk�FAi|�Ah�yAhv�AhA�Ag"�Ae`BAc+AbffAat�A`��A_"�A^1'A]�A\Q�A[�#A[O�A[AZ��AZ��AZA�AY��AXAVbAUƨAUoAT1AR��AQp�AO�mAN�ANn�AMVAK+AIK�AH�yAG
=AC��AC%AA�TAAK�AA+A@�A@^5A?��A>ĜA<��A9�A7��A7�;A7��A6��A5�A4��A4VA3/A2�\A1�PA0�9A0��A/�A-��A-%A,5?A+�-A+p�A+VA*jA)hsA(�DA(1'A&��A%��A$jA#��A#�-A#G�A"��A ��A�TAhsA�`AffA9XA��A�`A��AbNAQ�A��A�\A1A�7A�HA9XAA�A�A��Ap�A�A�AO�A33A
�HA
z�A	�hA�!A$�At�A%A�!A�
AG�AA�RA�
A�+A�-AG�@�ƨ@�=q@�A�@�o@�5?@�/@�b@�~�@�p�@��@��9@���@��u@�j@�Q�@�Q�@�(�@�dZ@�!@�=q@�^@��@��@�V@�9@��@��T@��/@�1'@�P@�"�@�E�@�7@��u@��@ݑh@܃@ۍP@���@ڏ\@�E�@�{@٩�@ى7@�hs@�7L@���@�Ĝ@�Z@�t�@���@�J@�1@�%@�hs@�1'@ˮ@���@�V@�;d@°!@�E�@�J@��^@���@�1@�l�@�C�@�+@��@�~�@�@�/@�Z@�9X@� �@�  @��F@�+@�A�@�ƨ@�M�@��7@���@�j@�o@�5?@�x�@��9@�j@� �@�ff@���@��u@���@���@��@��@��9@�ȴ@�J@��@���@�/@�1'@�+@�@��@��@���@�v�@���@��^@�hs@���@��@��@��@��@�x�@�?}@���@���@�z�@�1@���@�t�@�o@�v�@�ff@�M�@�J@��7@�I�@�;d@��H@��H@���@�~�@��@�O�@�/@��@���@�Ĝ@�r�@�Z@�A�@� �@��@��@���@���@�o@��^@�O�@��@�j@��@��m@��w@��F@���@���@�|�@�\)@�K�@��@���@�$�@�bN@�t�@�@��@���@���@��R@��!@���@�ff@�-@�J@��@��^@�O�@�V@��@�ƨ@��@�\)@�o@���@�M�@��@��9@�b@�1@�  @�  @���@��m@��w@�t�@�ȴ@���@��D@� �@��@l�@\)@�@~E�@~$�@}�-@}�@}�@}p�@}�-@~�+@;d@~��@~��@~@}O�@z��@y�@y��@y��@x�u@w��@wl�@t��@lj@d�@^{@T��@M�@Ihs@C��@?\)@7\)@1X@*^5@$��@ �9@�@��@^5@+@
�\@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�33A�33A�33A�5?A�1'A���A���A�jA�"�A���A��mA���A���A��A�r�A�dZA��A���A��A�9XA�{A�I�A�?}A�(�A���A�ƨA��PA�I�A�S�A�^5A�$�A�{A���A���A��wA���A�ffA��A��A���A�VA���A���A��A���A���A�JA��A�{A�ZA��`A��A�
=A�ZA��;A�|�A�hsA�Q�A�x�A�33A��+A��#A��A�ffA��mA���A���A��A��9A�t�A�&�A��`A���A�1'A�
=A���A��yA��#A��FA��uA�=qA��wA�1'A�M�A��A�ĜA���A�Q�A��`A�v�A�1A��^A�(�A��`A�x�A�l�A�O�A�7LA�"�A��A�A�A�A���A��A���A�A�C�A���A�ZA�z�A���A�p�A���A�S�A��#A��hA���A��+A��DA���A�l�A��A~�A~��A}��A|��Az-Ayt�Ax  AvbNAu��As��Ar�Aq�Ap=qAoAk�FAi|�Ah�yAhv�AhA�Ag"�Ae`BAc+AbffAat�A`��A_"�A^1'A]�A\Q�A[�#A[O�A[AZ��AZ��AZA�AY��AXAVbAUƨAUoAT1AR��AQp�AO�mAN�ANn�AMVAK+AIK�AH�yAG
=AC��AC%AA�TAAK�AA+A@�A@^5A?��A>ĜA<��A9�A7��A7�;A7��A6��A5�A4��A4VA3/A2�\A1�PA0�9A0��A/�A-��A-%A,5?A+�-A+p�A+VA*jA)hsA(�DA(1'A&��A%��A$jA#��A#�-A#G�A"��A ��A�TAhsA�`AffA9XA��A�`A��AbNAQ�A��A�\A1A�7A�HA9XAA�A�A��Ap�A�A�AO�A33A
�HA
z�A	�hA�!A$�At�A%A�!A�
AG�AA�RA�
A�+A�-AG�@�ƨ@�=q@�A�@�o@�5?@�/@�b@�~�@�p�@��@��9@���@��u@�j@�Q�@�Q�@�(�@�dZ@�!@�=q@�^@��@��@�V@�9@��@��T@��/@�1'@�P@�"�@�E�@�7@��u@��@ݑh@܃@ۍP@���@ڏ\@�E�@�{@٩�@ى7@�hs@�7L@���@�Ĝ@�Z@�t�@���@�J@�1@�%@�hs@�1'@ˮ@���@�V@�;d@°!@�E�@�J@��^@���@�1@�l�@�C�@�+@��@�~�@�@�/@�Z@�9X@� �@�  @��F@�+@�A�@�ƨ@�M�@��7@���@�j@�o@�5?@�x�@��9@�j@� �@�ff@���@��u@���@���@��@��@��9@�ȴ@�J@��@���@�/@�1'@�+@�@��@��@���@�v�@���@��^@�hs@���@��@��@��@��@�x�@�?}@���@���@�z�@�1@���@�t�@�o@�v�@�ff@�M�@�J@��7@�I�@�;d@��H@��H@���@�~�@��@�O�@�/@��@���@�Ĝ@�r�@�Z@�A�@� �@��@��@���@���@�o@��^@�O�@��@�j@��@��m@��w@��F@���@���@�|�@�\)@�K�@��@���@�$�@�bN@�t�@�@��@���@���@��R@��!@���@�ff@�-@�J@��@��^@�O�@�V@��@�ƨ@��@�\)@�o@���@�M�@��@��9@�b@�1@�  @�  @���@��m@��w@�t�@�ȴ@���@��D@� �@��@l�@\)@�@~E�@~$�@}�-@}�@}�@}p�@}�-@~�+@;d@~��@~��@~@}O�@z��@y�@y��@y��@x�u@w��@wl�@t��@lj@d�@^{@T��@M�@Ihs@C��@?\)@7\)@1X@*^5@$��@ �9@�@��@^5@+@
�\@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�VB�VB�VB�VB�JB�7B�B~�B|�B|�B|�B|�B}�B}�B}�B|�B{�Bx�Bx�Bu�Bm�BffBiyBjBm�Bk�BgmBdZBjBp�Bo�Bs�Bs�Bs�Br�Bq�Bn�Bk�BgmBffB_;B_;B`BB^5BVBL�B@�B8RB/B�BJB��B�B�`B�5B�#B�5B�TB�B�mB�B�}B��B��B�{Bv�BaHB[#BW
BXBR�BO�BI�BC�B@�B?}B>wB<jB9XB6FB0!B%�B�B
=BB  B��B��B�B�yB�TB�;B�
B��BĜBB�wB�dB�XB�RB�FB�?B�9B�B��B��B�PB�Bt�Bm�B^5BQ�BG�B=qB1'B%�B�BbBB
�B
�BB
ǮB
�jB
�LB
�9B
�B
��B
�+B
� B
p�B
aHB
XB
G�B
0!B
&�B
�B
JB	�B	�`B	�HB	�/B	�#B	��B	ǮB	�jB	�LB	�-B	�B	��B	��B	��B	��B	�{B	�oB	�bB	�\B	�PB	�DB	�+B	|�B	m�B	hsB	`BB	XB	N�B	F�B	=qB	6FB	2-B	)�B	!�B	�B	�B	bB	B��B��B��B��B��B�B�B�mB�5B��B��B��B��B��BɺBǮBĜBBBB�wBȴB��B��B��BɺBǮBƨBĜB��B�qB�^B�LB�-B�B�B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�PB�JB�=B�1B�B�B�B~�B|�Bw�Bs�Bq�Bm�BhsBbNB\)BVBJ�BE�BC�B@�B=qB<jB;dB;dB:^B5?B33B2-B49B1'B/B.B+B&�B#�B"�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoB{B�B�B�B�B�B!�B"�B#�B#�B#�B$�B&�B'�B'�B'�B'�B(�B)�B)�B,B,B,B+B+B)�B/B0!B7LB:^B:^B<jB@�BB�BD�BG�BG�BG�BM�BR�BS�BR�BT�BXBXBYB`BBdZBdZBe`BffBjBn�Bn�Bn�Bn�Bn�Bn�Bp�Bq�Br�Bt�Bw�Bz�B|�B� B�B�B�B�%B�%B�1B�1B�JB�VB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�'B�'B�-B�-B�3B�?B�}BBÖBȴB��B��B��B��B��B��B��B��B��B��B��B�
B�BB�fB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	  B	B	B	B	B	PB	oB	uB	uB	uB	uB	uB	{B	{B	�B	�B	$�B	(�B	)�B	+B	+B	,B	0!B	0!B	2-B	33B	33B	49B	7LB	;dB	>wB	@�B	A�B	B�B	B�B	C�B	B�B	B�B	D�B	F�B	I�B	L�B	ZB	� B	�B	�qB	�B	�B	��B
JB
�B
"�B
/B
;dB
D�B
J�B
P�B
YB
]/B
cTB
iyB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�9B�;B�;B�9B�;B�/B�B�B~�B|�B|�B|�B|�B}�B}�B}�B|�B{�Bx�Bx�Bu�BmsBfIBiZBj`BmsBkcBgMBd<BjcBp�Bo|Bs�Bs�Bs�Br�Bq�BnvBkgBgLBfFB_B_B`"B^BU�BL�B@`B82B.�BB*B��B�|B�AB�B��B�B�2B�bB�MB��B�\B��B�nB�WBv�Ba$BZ�BV�BW�BR�BO�BI�BCrB@^B?YB>SB<BB9.B6"B/�B%�BxB
B�B��B��B��B�xB�RB�+B�B��BϻB�xB�lB�RB�@B�.B�*B�B�B�B��B��B�bB�+B��Bt�BmlB^BQ�BG�B=OB1B%�B�B?B�B
��B
�B
ǌB
�EB
�(B
�B
��B
��B
�B
�B
p�B
a'B
W�B
G�B
0B
&�B
sB
+B	�B	�@B	�)B	�B	�B	��B	ǐB	�KB	�.B	�B	��B	��B	��B	��B	�oB	�^B	�RB	�EB	�@B	�3B	�(B	�B	|�B	muB	hZB	`%B	W�B	N�B	F�B	=VB	6-B	2B	)�B	!�B	�B	rB	GB	B��B��B��B��B��B�B�rB�UB�B��B��B��BͺB˯BɥBǕBąB�wB�xB�vB�aBțB��BͺBˬBɢBǓBƏBĄB�pB�YB�GB�5B�B�B��B��B��B��B��B��B�|B�uB�iB�dB�XB�LB�@B�;B�3B�&B�B�	B��B��B~�B|�Bw�Bs�Bq�Bm~Bh]Bb:B\BU�BJ�BE�BC�B@mB=]B<VB;OB;NB:HB5*B3B2B4%B1B/B-�B*�B&�B#�B"�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�BwBvB�B�B}BlB�BbB�BcB~BcByBrBYBnBlBQBjBOBVBXBWBlBrBXBXBXBrBRBRBkBJBEB=BJB~B�BvB{B�B!�B"�B#�B#�B#�B$�B&�B'�B'�B'�B'�B(�B)�B)�B+�B+�B+�B*�B*�B)�B/B0
B72B:FB:GB<SB@jBBwBD�BG�BG�BG�BM�BR�BS�BR�BT�BW�BW�BX�B`)Bd>Bd<BeCBfJBjcBn}Bn|Bn}Bn}Bn~Bn}Bp�Bq�Br�Bt�Bw�Bz�B|�B�B��B��B��B�B�B�B�B�.B�8B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�B�B�B�B�B�]B�oB�tBȒB̬B̮BͱBͱBιBηBϾBϿB��B��B��B��B�"B�BB�ZB�ZB�bB�dB�bB�dB�hB�pB�uB�{B��B�B��B��B��B��B��B��B	 �B	 �B	�B	�B	,B	MB	PB	PB	PB	PB	NB	VB	WB	\B	wB	$�B	(�B	)�B	*�B	*�B	+�B	/�B	/�B	2B	3
B	3B	4B	7%B	;@B	>SB	@]B	AcB	BhB	BiB	CqB	BiB	BgB	DxB	F�B	I�B	L�B	Y�B	�B	��B	�FB	��B	�ZB	��B
!B
SB
"�B
.�B
;8B
DnB
J�B
P�B
X�B
]B
c(B
iKB
me11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150517191625    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150517191625  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150517191625  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                