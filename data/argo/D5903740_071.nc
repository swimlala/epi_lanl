CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:42Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230842  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               GA   AO  4055_7112_071                   2C  D   APEX                            5374                            041511                          846 @��vT 1   @��r�@:�l�C���d���l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    GA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyS3D��D�C3D�y�D���D��D�FfD�y�D�s3D�fD�,�D��3D��3D�fD�33D�p D� D��D�FfD�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��Dyk�D��D�O\D���D���D��D�R�D���D�\D�"�D�8�D��\D��\D��D�?\D�|)D�)D��D�R�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�$�A���A��yA���A�ĜA��-A�z�A� �A�  A���A��A���A��A��A��`A��mA��/A��/A��
A���A���A�ĜA��A���A���A��uA��PA��7A�v�A�ZA�/A��A�JA��A��^A�n�A�hsA�ffA�dZA�\)A�ZA�ZA�O�A�;dA�33A�-A�oA��A��mA���A��!A���A��DA�n�A�ffA�jA�bNA�Q�A���A��A���A�A�|�A��A��jA���A�n�A�bA�oA���A�l�A�oA�ĜA��\A�n�A���A��7A�(�A�
=A��+A�1'A�v�A��TA���A���A�jA��uA�  A��A��A��A�"�A�  A���A�VA���A���A�r�A�A�A��9A�/A~bAz�Aw��Aup�AtbNAs�PAr�ArVApjAljAk�Ah�Af �Ad  AbI�AaK�A`��A_O�A^ZA\z�A[XAZ{AY�7AX�RAW|�AU"�AQ�
APȴAO��ANbAMl�AL�ALA�AKp�AJ�AJAJ1AI��AH�yAG;dAE�AD��AD$�AC��AB��AAXA?�A?A=��A<r�A;`BA:��A9�7A7ƨA6�\A6�A5�A5�TA5��A5��A5�hA5C�A4��A4��A4Q�A3XA2�uA1ƨA1oA.�`A-A,ZA+�A+��A+K�A*�A(�A(�A'|�A&��A&~�A&ZA&-A%;dA"r�A�TA
=A��AA1'A
=A�9A�TA�yA��A��Av�A�A1A�^A?}AVA(�AS�A�Ap�A
�A
�uA
(�A	��A	�-A	�hA	K�A�-A��AffAQ�AE�A��A?}A&�A��A�RA�AjA1A�A��A��At�A?}A��A��A�\A�AK�A �!A ^5A 1@�@�%@���@�Z@���@���@��@�  @��;@���@��
@��
@�ƨ@��m@�@���@�ƨ@�33@��@�v�@��@�`B@�z�@ߥ�@�K�@ޏ\@��T@��@���@�r�@�A�@�1'@�K�@��@٩�@�%@�9X@�$�@���@�o@щ7@��@�I�@ϝ�@Ο�@�{@˕�@ɲ-@�&�@��;@�{@�@ř�@�x�@�?}@���@�z�@�ƨ@���@��@�Z@��@�M�@�/@���@�j@�I�@��@���@��@��@��T@���@��@�o@�ff@���@�ƨ@��@�t�@�K�@�C�@�@�hs@�?}@�/@��@���@�bN@�\)@���@�^5@�-@�%@��@�ƨ@�33@�
=@���@��+@�{@�hs@��j@���@���@��u@��u@�j@�I�@�9X@� �@�(�@��;@��w@�l�@���@�{@��@���@�ƨ@�33@�J@���@�|�@���@�=q@��h@��@���@���@���@��@�\)@�ȴ@�^5@�{@��-@���@�bN@��
@��@��P@�l�@��@���@�ȴ@���@��R@���@��@�V@�ƨ@��@�E�@��#@���@�hs@�&�@���@�Ĝ@���@��D@� �@���@�l�@�dZ@�dZ@��@�J@�X@��`@��9@�z�@�  @�t�@�+@�
=@�@���@���@��@��@�5?@���@�G�@�%@��@��m@��F@�t�@�C�@�+@�"�@��R@���@�V@���@��^@�p�@�/@�%@���@��/@��j@��@���@�z�@�1'@�  @�;@�P@\)@
=@~V@}��@}O�@}�@|�@|z�@|1@{�@{C�@{o@z�!@z��@z~�@zn�@z^5@z^5@zM�@z=q@z�@y�7@x��@x�9@x�@xbN@x  @w��@w�@w��@w\)@w�@v�R@vv�@v5?@u�-@p�@n5?@c@[t�@S33@L�/@H�u@C@;S�@3"�@-�T@)��@&��@$z�@��@A�@�F@�w@@p�@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�x�A�$�A���A��yA���A�ĜA��-A�z�A� �A�  A���A��A���A��A��A��`A��mA��/A��/A��
A���A���A�ĜA��A���A���A��uA��PA��7A�v�A�ZA�/A��A�JA��A��^A�n�A�hsA�ffA�dZA�\)A�ZA�ZA�O�A�;dA�33A�-A�oA��A��mA���A��!A���A��DA�n�A�ffA�jA�bNA�Q�A���A��A���A�A�|�A��A��jA���A�n�A�bA�oA���A�l�A�oA�ĜA��\A�n�A���A��7A�(�A�
=A��+A�1'A�v�A��TA���A���A�jA��uA�  A��A��A��A�"�A�  A���A�VA���A���A�r�A�A�A��9A�/A~bAz�Aw��Aup�AtbNAs�PAr�ArVApjAljAk�Ah�Af �Ad  AbI�AaK�A`��A_O�A^ZA\z�A[XAZ{AY�7AX�RAW|�AU"�AQ�
APȴAO��ANbAMl�AL�ALA�AKp�AJ�AJAJ1AI��AH�yAG;dAE�AD��AD$�AC��AB��AAXA?�A?A=��A<r�A;`BA:��A9�7A7ƨA6�\A6�A5�A5�TA5��A5��A5�hA5C�A4��A4��A4Q�A3XA2�uA1ƨA1oA.�`A-A,ZA+�A+��A+K�A*�A(�A(�A'|�A&��A&~�A&ZA&-A%;dA"r�A�TA
=A��AA1'A
=A�9A�TA�yA��A��Av�A�A1A�^A?}AVA(�AS�A�Ap�A
�A
�uA
(�A	��A	�-A	�hA	K�A�-A��AffAQ�AE�A��A?}A&�A��A�RA�AjA1A�A��A��At�A?}A��A��A�\A�AK�A �!A ^5A 1@�@�%@���@�Z@���@���@��@�  @��;@���@��
@��
@�ƨ@��m@�@���@�ƨ@�33@��@�v�@��@�`B@�z�@ߥ�@�K�@ޏ\@��T@��@���@�r�@�A�@�1'@�K�@��@٩�@�%@�9X@�$�@���@�o@щ7@��@�I�@ϝ�@Ο�@�{@˕�@ɲ-@�&�@��;@�{@�@ř�@�x�@�?}@���@�z�@�ƨ@���@��@�Z@��@�M�@�/@���@�j@�I�@��@���@��@��@��T@���@��@�o@�ff@���@�ƨ@��@�t�@�K�@�C�@�@�hs@�?}@�/@��@���@�bN@�\)@���@�^5@�-@�%@��@�ƨ@�33@�
=@���@��+@�{@�hs@��j@���@���@��u@��u@�j@�I�@�9X@� �@�(�@��;@��w@�l�@���@�{@��@���@�ƨ@�33@�J@���@�|�@���@�=q@��h@��@���@���@���@��@�\)@�ȴ@�^5@�{@��-@���@�bN@��
@��@��P@�l�@��@���@�ȴ@���@��R@���@��@�V@�ƨ@��@�E�@��#@���@�hs@�&�@���@�Ĝ@���@��D@� �@���@�l�@�dZ@�dZ@��@�J@�X@��`@��9@�z�@�  @�t�@�+@�
=@�@���@���@��@��@�5?@���@�G�@�%@��@��m@��F@�t�@�C�@�+@�"�@��R@���@�V@���@��^@�p�@�/@�%@���@��/@��j@��@���@�z�@�1'@�  @�;@�P@\)@
=@~V@}��@}O�@}�@|�@|z�@|1@{�@{C�@{o@z�!@z��@z~�@zn�@z^5@z^5@zM�@z=q@z�@y�7@x��@x�9@x�@xbN@x  @w��@w�@w��@w\)@w�@v�R@vv�@v5?@u�-@p�@n5?@c@[t�@S33@L�/@H�u@C@;S�@3"�@-�T@)��@&��@$z�@��@A�@�F@�w@@p�@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB(�BF�BG�BG�BG�BG�BG�BH�BM�BO�BP�BQ�BP�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BT�BT�BT�BVBVBVBVBVBVBVBT�BT�BS�BP�BL�BF�BL�BN�BN�BN�BN�BM�BM�BK�BI�BH�BF�BC�BB�B@�B=qB;dB:^B7LB6FB8RB7LB49B%�BhB+BB��B�B�BBɺB�LB�hBr�BM�BhB�B��B�9B��Br�B\)BT�BQ�BF�B=qB�B
��B
�ZB
�B
��B
ǮB
�}B
�LB
�B
��B
�bB
� B
n�B
e`B
aHB
^5B
[#B
W
B
N�B
E�B
49B
�B
JB	��B	��B	�B	�B	�fB	�B	��B	�LB	��B	��B	�\B	�%B	�B	|�B	t�B	n�B	ffB	aHB	\)B	ZB	W
B	O�B	?}B	0!B	/B	+B	$�B	 �B	�B	�B	uB	\B	VB	PB	DB	+B	B	  B��B��B��B�B�B�mB�TB�;B�B��B��B��B��BȴBǮBǮBƨBƨBƨBŢBĜBÖBB��B��B�wB�jB�LB�!B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�JB�B{�Bs�Bn�Bk�BffBdZBaHB_;B\)BYBXBW
BVBT�BR�BN�BJ�BH�BF�BD�BB�BA�B@�B?}B?}B>wB=qB;dB8RB7LB7LB6FB6FB49B49B49B33B33B2-B2-B1'B1'B0!B0!B/B/B.B-B,B+B(�B'�B&�B%�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B#�B#�B#�B#�B#�B#�B$�B(�B-B1'B1'B49B5?B5?B5?B6FB6FB7LB8RB8RB9XB8RB9XB:^B:^BB�BC�BC�BC�BB�BG�BJ�BK�BK�BL�BL�BN�BQ�BS�BS�BS�BW
B\)B\)B_;B_;B`BBaHBcTBe`BiyBiyBiyBjBjBk�Bl�Bl�Bl�Bl�Bn�Bn�Bp�Bs�Bv�Bv�Bz�B�B�B�7B�bB��B��B��B��B��B��B��B��B��B�B�3B�?B�LB�XB�wBBŢBƨBǮBȴB��B��B��B��B��B��B��B�
B�HB�mB�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	PB	uB	�B	�B	�B	�B	#�B	%�B	'�B	'�B	'�B	'�B	'�B	'�B	.B	33B	6FB	8RB	:^B	@�B	A�B	D�B	E�B	F�B	F�B	I�B	J�B	N�B	P�B	Q�B	T�B	W
B	YB	ZB	[#B	\)B	]/B	]/B	^5B	aHB	cTB	dZB	e`B	ffB	gmB	jB	m�B	o�B	p�B	q�B	s�B	u�B	w�B	x�B	y�B	{�B	{�B	|�B	|�B	|�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�7B	�=B	�DB	�PB	��B	�B	��B	�sB	��B
JB
�B
�B
'�B
49B
<jB
A�B
E�B
J�B
Q�B
XB
_;B
dZB
jB
q�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B(�BF�BG�BG�BG�BG�BG�BH�BM�BO�BP�BQ�BP�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BT�BT�BT�BU�BU�BU�BU�BU�BU�BU�BT�BT�BS�BP�BL�BF�BL�BN�BN�BN�BN�BM�BM�BK�BI�BH�BF�BCtBBnB@bB=NB;CB:=B7-B6(B80B7*B4B%�BFBB�B��B�tB�"BɚB�+B�CBr�BM�BCB�sBϻB�B�sBr�B\BT�BQ�BF�B=OB�B
��B
�:B
��B
��B
ǊB
�\B
�)B
��B
�{B
�?B
�B
nuB
e?B
a&B
^B
[B
V�B
N�B
E�B
4B
�B
*B	��B	��B	��B	�hB	�IB	��B	�eB	�.B	��B	��B	�AB	�
B	��B	|�B	t�B	n}B	fHB	a.B	\B	ZB	V�B	O�B	?aB	0B	/ B	*�B	$�B	 �B	�B	yB	[B	AB	=B	6B	*B	B	�B��B��B��B��B�B�xB�VB�<B�#B�B��B��B��BʫBȝBǕBǖBƐBƏBƎBŌBćB�~B�yB�jB�mB�^B�RB�6B�
B��B��B��B��B��B��B��B��B�xB�pB�jB�cB�[B�3B�B{�Bs�Bn�BkpBfOBdEBa2B_$B\BYBW�BV�BU�BT�BR�BN�BJ�BH�BF�BD�BB{BAuB@nB?hB?iB>bB=\B;OB8=B76B79B61B60B4%B4%B4#B3B3B2B2B0�B1B0B0B.�B/	B-�B,�B+�B*�B(�B'�B&�B%�B#�B �B�B�B�B�BvBxBvByBxB�BeBWBXBXBwBzBwBwBxBvBuBwBvBxB]BvB]BvB_BWBVByBuBqBWBwBvBjBwB�BvBsB�B�B�B�B�B�B"�B#�B#�B#�B#�B#�B#�B#�B$�B(�B,�B1B1B4B5)B5'B5&B6*B6,B74B8;B8;B9AB89B9?B:CB:DBBwBC}BCBCzBBuBG�BJ�BK�BK�BL�BL�BN�BQ�BS�BS�BS�BV�B\B\B_B_ B`'Ba,Bc7BeDBi[Bi\Bi]BjfBjfBkkBlmBloBlpBlmBn{Bn{Bp�Bs�Bv�Bv�Bz�B��B��B�B�DB�hB��B��B��B��B��B��B��B��B��B�B� B�-B�7B�WB�pBŀBƆBǋBȕBʡB̮B̬B̭B̭B̭B��B��B�%B�KB�hB��B�B�B��B��B��B��B��B��B	�B	�B	�B	�B	�B	-B	PB	iB	oB	}B	�B	#�B	%�B	'�B	'�B	'�B	'�B	'�B	'�B	-�B	3B	6 B	8,B	:6B	@^B	AfB	DvB	E}B	F�B	F�B	I�B	J�B	N�B	P�B	Q�B	T�B	V�B	X�B	Y�B	Z�B	\B	]B	]B	^B	a"B	c/B	d1B	e8B	f?B	gFB	jWB	mhB	oxB	p{B	q�B	s�B	u�B	w�B	x�B	y�B	{�B	{�B	|�B	|�B	|�B	|�B	}�B	}�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	��B	��B	��B	�JB	��B
"B
SB
�B
'�B
4B
<?B
A[B
EtB
J�B
Q�B
W�B
_B
d+B
jSB
q~B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230842  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230842  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                