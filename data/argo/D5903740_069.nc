CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:41Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230841  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               EA   AO  4055_7112_069                   2C  D   APEX                            5374                            041511                          846 @�ۖ��	1   @�ۗ9�`@;T9XbN�d$bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    EA   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy9�D��D�Y�D��3D��fD�3D�<�D�� D���D��fD�VfD���D��fD���D�9�D�y�D�ɚD�fD�@ D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@���A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR��DSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��DyQ�D��D�e�D��\D�ҏD�\D�H�D��)D���D��D�b�D���D��D��D�E�Dڅ�D���D�"�D�L)D�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�VA�XA�XA�ZA�ZA�VA�VA�VA�XA�ZA�ZA�ZA�^5A�S�A�Q�A�E�A�A�A�E�A�A�A�C�A�A�A�?}A�;dA�5?A�=qA���A���A��A��`A��`A��#A���A���A���A���A�ȴA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�A�A�A���A��jA��jA��jA��wA��wA��wA��wA��wA��RA���A�r�A��mA���A���A�~�A���A�ffA�=qA�7LA�33A�/A�/A�/A�/A�+A�ffA�Q�A���A��A�jA�E�A�n�A�\)A��9A��HA���A��mA�9XA�O�A��7A�z�A� �A��A�$�A���A�?}A�t�A�A�l�A�$�A~ �A|v�A{�A{�Aw��AvAs|�Aq|�AnZAjn�Ah��Af�DAfv�Ae�Ad�Ad�DAd  Ac�Ac+Ab�/Ab�+AbVAa��A`�A`�A^�A^ffA^A]
=A\9XA[�;A[�^A[�FA[�7A[XAZn�AXI�AW�PAVM�ATA�AR�/AQVAOK�AK7LAI�AF�+ADz�AC�#AC��AC��AA�TA@�/A@�A@1'A?�#A?&�A>�A=dZA=
=A:�A:9XA9�TA9+A8�uA81A7��A7G�A5�TA5x�A5K�A5VA3�A2��A2�A1��A1��A0��A0(�A/�mA/�^A/"�A.E�A-�hA,�A+t�A+O�A+"�A*�uA*5?A)�A)�FA)�hA)�A)t�A)G�A(�RA(~�A'�A%l�A#��A#�-A#
=A ��A �A�DAZA�FA|�AK�A��A�
A�HAjA�mAn�Ax�AG�A��A��A�A$�A�A��A��A7LA��A�A�A�AVA(�A�wA�!A �AAA��A��At�AoAĜAn�A�TA��A|�A+A
ĜA
^5A
=qA
�A	ƨA	?}AM�A^5A��AjA(�A��Ax�A�A%@��\@���@���@���@�(�@�?}@�{@�D@�w@�l�@�S�@�C�@��H@�j@�@�|�@�@�D@�"�@���@���@݉7@ۥ�@���@�(�@ו�@���@��@թ�@�V@�Ĝ@�I�@�33@�`B@Ѓ@϶F@�"�@�$�@�O�@̼j@�r�@�Q�@��@�  @��@˥�@�;d@ʧ�@�bN@���@�Ĝ@�@��h@��j@��F@�z�@��@�5?@��@��#@���@���@�j@� �@�1@���@�ƨ@�C�@��@��@�33@��H@��\@�@�&�@�b@�n�@��@�dZ@�M�@��@�v�@�-@��@�{@���@��-@��@�9X@�+@�~�@�E�@��@���@���@�1'@�  @��F@�dZ@��y@�5?@���@�hs@�&�@�%@��@�9X@��w@�
=@�n�@�E�@�J@�@�X@�z�@�ƨ@��@���@���@��P@�|�@�l�@�l�@�dZ@�K�@�"�@�@���@��\@�ff@���@�hs@���@���@�bN@��@�|�@�o@��T@��@�X@��@���@�9X@� �@��@��F@��@�t�@�\)@�;d@��R@�@��T@��-@��7@�`B@�%@��D@��@�Z@�  @���@��F@�K�@�
=@�n�@�@�x�@��@�r�@�I�@��w@�l�@�o@��R@�M�@�@��#@�@��-@�x�@�X@�?}@�%@��/@��j@���@�j@��w@�S�@�+@���@�ȴ@���@�~�@�5?@�x�@��@�%@���@���@���@��j@��9@��9@���@��u@��D@�z�@�Z@�Q�@�9X@;d@~�y@~�R@~E�@}�@|�@|(�@|1@{��@{�F@w�@n�+@d�j@]V@V�y@P�9@K��@G��@@��@:~�@/��@)�^@&ff@"^5@5?@�9@�/@l�@"�@$�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�G�A�I�A�K�A�K�A�K�A�K�A�M�A�VA�XA�XA�ZA�ZA�VA�VA�VA�XA�ZA�ZA�ZA�^5A�S�A�Q�A�E�A�A�A�E�A�A�A�C�A�A�A�?}A�;dA�5?A�=qA���A���A��A��`A��`A��#A���A���A���A���A�ȴA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�A�A�A���A��jA��jA��jA��wA��wA��wA��wA��wA��RA���A�r�A��mA���A���A�~�A���A�ffA�=qA�7LA�33A�/A�/A�/A�/A�+A�ffA�Q�A���A��A�jA�E�A�n�A�\)A��9A��HA���A��mA�9XA�O�A��7A�z�A� �A��A�$�A���A�?}A�t�A�A�l�A�$�A~ �A|v�A{�A{�Aw��AvAs|�Aq|�AnZAjn�Ah��Af�DAfv�Ae�Ad�Ad�DAd  Ac�Ac+Ab�/Ab�+AbVAa��A`�A`�A^�A^ffA^A]
=A\9XA[�;A[�^A[�FA[�7A[XAZn�AXI�AW�PAVM�ATA�AR�/AQVAOK�AK7LAI�AF�+ADz�AC�#AC��AC��AA�TA@�/A@�A@1'A?�#A?&�A>�A=dZA=
=A:�A:9XA9�TA9+A8�uA81A7��A7G�A5�TA5x�A5K�A5VA3�A2��A2�A1��A1��A0��A0(�A/�mA/�^A/"�A.E�A-�hA,�A+t�A+O�A+"�A*�uA*5?A)�A)�FA)�hA)�A)t�A)G�A(�RA(~�A'�A%l�A#��A#�-A#
=A ��A �A�DAZA�FA|�AK�A��A�
A�HAjA�mAn�Ax�AG�A��A��A�A$�A�A��A��A7LA��A�A�A�AVA(�A�wA�!A �AAA��A��At�AoAĜAn�A�TA��A|�A+A
ĜA
^5A
=qA
�A	ƨA	?}AM�A^5A��AjA(�A��Ax�A�A%@��\@���@���@���@�(�@�?}@�{@�D@�w@�l�@�S�@�C�@��H@�j@�@�|�@�@�D@�"�@���@���@݉7@ۥ�@���@�(�@ו�@���@��@թ�@�V@�Ĝ@�I�@�33@�`B@Ѓ@϶F@�"�@�$�@�O�@̼j@�r�@�Q�@��@�  @��@˥�@�;d@ʧ�@�bN@���@�Ĝ@�@��h@��j@��F@�z�@��@�5?@��@��#@���@���@�j@� �@�1@���@�ƨ@�C�@��@��@�33@��H@��\@�@�&�@�b@�n�@��@�dZ@�M�@��@�v�@�-@��@�{@���@��-@��@�9X@�+@�~�@�E�@��@���@���@�1'@�  @��F@�dZ@��y@�5?@���@�hs@�&�@�%@��@�9X@��w@�
=@�n�@�E�@�J@�@�X@�z�@�ƨ@��@���@���@��P@�|�@�l�@�l�@�dZ@�K�@�"�@�@���@��\@�ff@���@�hs@���@���@�bN@��@�|�@�o@��T@��@�X@��@���@�9X@� �@��@��F@��@�t�@�\)@�;d@��R@�@��T@��-@��7@�`B@�%@��D@��@�Z@�  @���@��F@�K�@�
=@�n�@�@�x�@��@�r�@�I�@��w@�l�@�o@��R@�M�@�@��#@�@��-@�x�@�X@�?}@�%@��/@��j@���@�j@��w@�S�@�+@���@�ȴ@���@�~�@�5?@�x�@��@�%@���@���@���@��j@��9@��9@���@��u@��D@�z�@�Z@�Q�@�9X@;d@~�y@~�R@~E�@}�@|�@|(�@|1@{��@{�F@w�@n�+@d�j@]V@V�y@P�9@K��@G��@@��@:~�@/��@)�^@&ff@"^5@5?@�9@�/@l�@"�@$�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B{�B%�B�B�)BɺB��B�RB�-B�!B�B�!B�!B�!B�!B�!B�B��B{�Bm�BffBcTB^5BI�B,BB�
B�{B]/B%�B
��B
�B
�TB
�/B
��B
�qB
��B
iyB
\)B
E�B
?}B
:^B
+B
�B
�B
bB	��B	�B	�;B	��B	�qB	��B	��B	�uB	�oB	�\B	�PB	�DB	�7B	�+B	�B	�B	�B	�B	}�B	x�B	u�B	s�B	r�B	p�B	m�B	l�B	k�B	jB	jB	hsB	ffB	bNB	]/B	ZB	Q�B	N�B	M�B	C�B	5?B	�B	JB��B�B�B�B�B�TB�5B�)B�#B�B�B��B��B��B��B��BɺBǮBƨBŢBĜB��B�}B�wB�qB�dB�RB�FB�9B�9B�-B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�7B�+B�Bz�Bu�Bs�Br�Bp�Bo�Bn�Bl�BiyBgmBe`BbNB_;B]/B]/B\)B[#BZBZBYBXBW
BW
BVBT�BR�BP�BN�BL�BK�BJ�BI�BI�BI�BI�BH�BH�BG�BF�BE�BD�BD�BC�BB�BA�BA�B@�B?}B>wB<jB8RB49B2-B2-B1'B0!B.B,B(�B'�B&�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B#�B#�B)�B/B1'B2-B1'B33B49B49B49B49B49B49B49B6FB<jB>wB?}B@�BA�BC�BE�BI�BO�BR�BVBYBcTBdZBdZBdZBdZBe`BffBjBn�Bq�Br�Bs�Bu�B{�B~�B~�B�B�B�B�1B�DB�PB�VB�VB�bB�uB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�'B�'B�3B�?B�FB�RB�jB��B��BBǮBȴB��B��B�
B�B�B�#B�BB�BB�HB�TB�`B�`B�fB�fB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	DB	VB	uB	�B	�B	�B	!�B	#�B	'�B	+B	.B	1'B	33B	49B	49B	6FB	7LB	8RB	:^B	;dB	<jB	=qB	>wB	C�B	F�B	G�B	I�B	J�B	L�B	M�B	O�B	XB	ZB	[#B	[#B	]/B	]/B	^5B	_;B	_;B	`BB	aHB	bNB	bNB	dZB	dZB	e`B	jB	l�B	l�B	n�B	q�B	t�B	v�B	v�B	v�B	x�B	�B	�'B	��B	�mB	��B
B
DB
�B
!�B
+B
49B
?}B
D�B
H�B
M�B
VB
\)B
dZB
iyB
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B{�B%�B�B�BəB�bB�3B�B��B��B� B��B� B� B� B��B�mB{�BmmBf@Bc.B^BI�B+�B�B��B�WB]B%�B
��B
�B
�4B
�B
˨B
�NB
��B
iWB
\B
E�B
?\B
:@B
*�B
�B
{B
BB	��B	�xB	�B	��B	�SB	��B	��B	�YB	�QB	�?B	�5B	�*B	�B	�B	�B	��B	��B	��B	}�B	x�B	u�B	s�B	r�B	p�B	mvB	lqB	kjB	jdB	jdB	hYB	fJB	b3B	]B	ZB	Q�B	N�B	M�B	C|B	5$B	�B	1B��B��B�rB�~B�rB�=B�B�B�B��B��B��B��B��B˳B˯BɣBǕBƏBōBĆB�rB�gB�`B�WB�NB�:B�0B�"B�"B�B�	B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B�qB�TB�;B�#B�B��Bz�Bu�Bs�Br�Bp�Bo�Bn�BltBicBgVBeJBb9B_&B]B]B\B[BZBZBYBW�BV�BV�BU�BT�BR�BP�BN�BL�BK�BJ�BI�BI�BI�BI�BH�BH�BG�BF�BE�BD�BD�BC�BBzBArBAtB@oB?hB>cB<VB8>B4#B2B2B1B0B-�B+�B(�B'�B&�B#�B!�B�B�BpB�B�B�BqBjB|B^BxBYBXBmBXBWBYBRBRBvBxBwB|B�B~B�B}BBbB�BhBnB�BoBxBvB�B�B�B�B�B�B�B�BvB�B�B"�B#�B#�B#�B)�B/B1B2B1B3B4#B4"B4B4!B4 B4 B4 B6-B<QB>^B?fB@jBApBC|BE�BI�BO�BR�BU�BX�Bc7Bd=Bd=Bd>Bd?BeDBfKBjbBn}Bq�Br�Bs�Bu�B{�B~�B~�B��B��B�B�B�(B�2B�9B�7B�DB�YB�`B�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�$B�2B�IB�bB�iB�nBǍBȓBʡB��B��B��B��B�B�B�"B�'B�2B�>B�=B�AB�DB�`B�|B��B�B�B��B��B��B��B��B��B	 �B	 �B	�B	�B	B	0B	OB	iB	�B	�B	!�B	#�B	'�B	*�B	-�B	1B	3B	4B	4B	6B	7&B	8-B	:6B	;?B	<EB	=LB	>QB	CqB	F�B	G�B	I�B	J�B	L�B	M�B	O�B	W�B	Y�B	Z�B	Z�B	]
B	]B	^B	_B	_B	`B	a B	b'B	b'B	d4B	d1B	e8B	jUB	ldB	ldB	noB	q�B	t�B	v�B	v�B	v�B	x�B	��B	��B	̣B	�?B	��B
�B
B
UB
!�B
*�B
4B
?NB
DpB
H�B
M�B
U�B
[�B
d+B
iMB
pxB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230841  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230841  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                