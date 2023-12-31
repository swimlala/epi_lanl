CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-09T20:16:39Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160309201639  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_144                   2C  D   APEX                            5374                            041511                          846 @כ�QY��1   @כ��0�o@:l�C���c�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DyL�D��D�P D��fD��fD��D�FfD�� D�ٚD�  D�L�D�� D��fD�  D�33D�l�D��fD�	�D�0 D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @XQ�@�(�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2�D2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDyeD�%�D�\)D���D�D��D�R�D��)D���D�)D�X�D��)D��D�)D�?\D�x�D�ҏD��D�<)D�\D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�^5A�bNA�dZA�bNA�ffA�dZA�ffA�^5A�S�A�XA�^5A�VA�Q�A�S�A�M�A�/A��A��A�1A��A��HA��FA���A��hA�`BA�1'A��A�1A�S�A�-A�A��A��A�oA���A�bNA�9XA�&�A���A���A�\)A�K�A�A�A�
=A���A�E�A���A�t�A�9XA�t�A��FA��TA��A�E�A�1A��7A�$�A��#A�p�A�-A� �A�v�A��wA��;A��A��mA�t�A���A��
A�bNA���A�%A��A��\A�bNA��A��^A��A��9A��7A�t�A�1'A�ƨA�K�A�G�A��A�p�A��A��A���A��A�z�A�1'A���A��^A~��Az�Aw&�At��Ar��Ap�uAo�An�Amp�Al�Alv�Aj�Aj�AihsAh�HAg"�Ae�Ac�;Ac�Abn�AaC�A`ĜA`��A`bNA` �A_��A_l�A^bNA\��AZ��AYO�AY�AW�-AV�`AU"�ATVASK�AR��AP��AO\)AN�DAM�^AMG�AL��ALn�AK�#AK�AI/AH1AG`BAFbAEx�AEt�AEVADjACt�ABjAA��AA��AA�A?��A=?}A<9XA<A;�wA:E�A8��A733A6��A5��A5VA4��A41A3��A2��A1�A1p�A1`BA0�HA0jA0�A.�+A-�A,�A+ƨA+G�A*�A*(�A(�\A'�-A'S�A&�HA&JA%oA$-A#A"�A!G�A�#AdZA�AE�A�^A/AĜA1A�AG�AVAA�HA1A�A�A��A��A1'A��A�Az�A�PAXA"�A��A`BA^5Ap�A�!A=qA��A�yAE�AhsA
�A	ƨAhsAjAƨAC�A�HA��AjA{A��A��A�TAC�A Q�A $�@�|�@��@�1'@�1@���@�$�@��@�@���@�  @�G�@�@��@�M�@���@�`B@�+@�{@�V@�r�@�F@�ff@�^@���@�A�@���@�{@�G�@�A�@ڧ�@ٺ^@��@�O�@���@�bN@�"�@�V@�|�@���@�p�@̃@���@�J@�b@ǍP@�ff@Ƈ+@Ɨ�@�@Ł@�b@°!@�I�@��@�J@��h@��u@�b@��
@���@�S�@�^5@��7@��9@��@��@��R@�7L@���@��j@��@�"�@��#@�/@��u@��m@��@��+@��@�?}@�r�@���@�S�@��!@���@�V@�9X@�C�@��H@�-@�7L@�j@�b@�S�@�V@���@�/@�1@��@���@�v�@�^5@�{@��^@��D@�1'@�1'@�1'@�(�@�(�@�  @�+@�"�@��@��!@��@���@��@��7@�Ĝ@���@�S�@�ȴ@�^5@�-@���@��7@�`B@�Ĝ@�Z@�A�@�(�@�1'@��F@���@��@��y@��!@�~�@�V@��@��@��@�bN@�1@��@�33@��H@��R@�v�@�M�@��@���@�`B@�7L@��/@�z�@��@���@�dZ@�C�@�ȴ@���@�v�@�E�@�@��@��h@�Ĝ@��w@�33@�dZ@�S�@���@�M�@�-@�@��@��T@���@���@��7@�O�@�%@��j@���@��D@��@�z�@�Z@�A�@�9X@� �@���@���@�|�@�\)@�@���@���@���@��+@�ff@�$�@�@��#@��h@�x�@�hs@�O�@�?}@�7L@��@���@�bN@�9X@�(�@��@�@\)@~�y@~v�@~$�@}@}@}@}?}@|9X@{��@z��@z~�@z^5@z=q@y�#@y�^@yhs@y�@x�@w�w@w�@vȴ@vȴ@vff@u�T@s��@kt�@cS�@[dZ@U/@M�T@E?}@>v�@:��@4z�@-?}@&V@!G�@��@�@I�@x�@ȴ@
��@�P@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�^5A�bNA�dZA�bNA�ffA�dZA�ffA�^5A�S�A�XA�^5A�VA�Q�A�S�A�M�A�/A��A��A�1A��A��HA��FA���A��hA�`BA�1'A��A�1A�S�A�-A�A��A��A�oA���A�bNA�9XA�&�A���A���A�\)A�K�A�A�A�
=A���A�E�A���A�t�A�9XA�t�A��FA��TA��A�E�A�1A��7A�$�A��#A�p�A�-A� �A�v�A��wA��;A��A��mA�t�A���A��
A�bNA���A�%A��A��\A�bNA��A��^A��A��9A��7A�t�A�1'A�ƨA�K�A�G�A��A�p�A��A��A���A��A�z�A�1'A���A��^A~��Az�Aw&�At��Ar��Ap�uAo�An�Amp�Al�Alv�Aj�Aj�AihsAh�HAg"�Ae�Ac�;Ac�Abn�AaC�A`ĜA`��A`bNA` �A_��A_l�A^bNA\��AZ��AYO�AY�AW�-AV�`AU"�ATVASK�AR��AP��AO\)AN�DAM�^AMG�AL��ALn�AK�#AK�AI/AH1AG`BAFbAEx�AEt�AEVADjACt�ABjAA��AA��AA�A?��A=?}A<9XA<A;�wA:E�A8��A733A6��A5��A5VA4��A41A3��A2��A1�A1p�A1`BA0�HA0jA0�A.�+A-�A,�A+ƨA+G�A*�A*(�A(�\A'�-A'S�A&�HA&JA%oA$-A#A"�A!G�A�#AdZA�AE�A�^A/AĜA1A�AG�AVAA�HA1A�A�A��A��A1'A��A�Az�A�PAXA"�A��A`BA^5Ap�A�!A=qA��A�yAE�AhsA
�A	ƨAhsAjAƨAC�A�HA��AjA{A��A��A�TAC�A Q�A $�@�|�@��@�1'@�1@���@�$�@��@�@���@�  @�G�@�@��@�M�@���@�`B@�+@�{@�V@�r�@�F@�ff@�^@���@�A�@���@�{@�G�@�A�@ڧ�@ٺ^@��@�O�@���@�bN@�"�@�V@�|�@���@�p�@̃@���@�J@�b@ǍP@�ff@Ƈ+@Ɨ�@�@Ł@�b@°!@�I�@��@�J@��h@��u@�b@��
@���@�S�@�^5@��7@��9@��@��@��R@�7L@���@��j@��@�"�@��#@�/@��u@��m@��@��+@��@�?}@�r�@���@�S�@��!@���@�V@�9X@�C�@��H@�-@�7L@�j@�b@�S�@�V@���@�/@�1@��@���@�v�@�^5@�{@��^@��D@�1'@�1'@�1'@�(�@�(�@�  @�+@�"�@��@��!@��@���@��@��7@�Ĝ@���@�S�@�ȴ@�^5@�-@���@��7@�`B@�Ĝ@�Z@�A�@�(�@�1'@��F@���@��@��y@��!@�~�@�V@��@��@��@�bN@�1@��@�33@��H@��R@�v�@�M�@��@���@�`B@�7L@��/@�z�@��@���@�dZ@�C�@�ȴ@���@�v�@�E�@�@��@��h@�Ĝ@��w@�33@�dZ@�S�@���@�M�@�-@�@��@��T@���@���@��7@�O�@�%@��j@���@��D@��@�z�@�Z@�A�@�9X@� �@���@���@�|�@�\)@�@���@���@���@��+@�ff@�$�@�@��#@��h@�x�@�hs@�O�@�?}@�7L@��@���@�bN@�9X@�(�@��@�@\)@~�y@~v�@~$�@}@}@}@}?}@|9X@{��@z��@z~�@z^5@z=q@y�#@y�^@yhs@y�@x�@w�w@w�@vȴ@vȴ@vff@u�T@s��@kt�@cS�@[dZ@U/@M�T@E?}@>v�@:��@4z�@-?}@&V@!G�@��@�@I�@x�@ȴ@
��@�P@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB7LB7LB7LB7LB7LB7LB6FB6FB9XB9XB:^B:^B:^B:^B:^B;dB<jB=qB=qB=qB>wB>wB>wB?}B>wB@�BA�BH�BN�BJ�B>wB.B+B �BuBDB%BBB��B��B��B��B��B�B�B�`B�B�wB��Bz�Bp�BXBB�B+B�B{B\B
=BB  B��B�B�sB�#B��B�dB��B��B�bB�=B� BiyB]/BS�BM�B8RB$�B�BhBPB
=BB
��B
��B
�mB
ȴB
��B
o�B
jB
�oB
��B
��B
��B
��B
�VB
x�B
Q�B
1'B
�B
+B	��B	�B	�B	�sB	�fB	�B	�B	�B	�B	�B	�;B	�
B	��B	ĜB	�wB	�dB	�^B	�XB	�LB	�?B	�-B	�B	��B	��B	�uB	�VB	�JB	�%B	~�B	t�B	n�B	hsB	cTB	ZB	S�B	O�B	L�B	J�B	H�B	D�B	A�B	:^B	,B	"�B	�B	VB	DB	VB	JB	JB	1B	B��B��B��B�B�fB�`B�mB�sB�;B��B��BȴBŢBBBB��B�qB�dB�^B�^B�jB�wB�qB�jB�^B�FB�3B�!B�B��B��B��B��B��B��B��B��B�uB�bB�JB�=B�1B�%B�%B�B�B�B~�B|�B{�Bz�Bv�Bt�Br�Bp�Bo�Bo�Bn�Bl�Bk�BiyBffBdZBcTBaHB^5BZBVBT�BR�BP�BN�BL�BJ�BG�BE�BA�B>wB<jB;dB;dB;dB;dB:^B9XB8RB6FB5?B33B33B33B2-B0!B/B.B-B,B)�B(�B&�B%�B$�B&�B%�B$�B"�B#�B$�B$�B$�B#�B"�B#�B#�B#�B#�B#�B$�B$�B$�B%�B%�B(�B(�B(�B'�B%�B&�B(�B'�B)�B)�B)�B,B/B1'B5?B>wBE�BI�BG�BD�BA�B@�BA�BD�BB�BA�BB�BB�BB�BA�B@�B@�B@�BA�BC�BB�BF�BG�BF�BH�BJ�BN�BO�BQ�BR�BVBVBW
BYB[#B`BBbNBe`BgmBiyBl�Bp�Bp�Br�Bv�Bx�Bx�Bz�B~�B�B�B�1B�PB�VB�bB�bB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�?B�XB�jB��BƨBɺB��B��B��B�B�#B�/B�5B�5B�NB�ZB�mB�yB�B�B��B��B��B��B	  B	B	B	%B	1B	DB	JB	VB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	'�B	.B	49B	6FB	8RB	>wB	@�B	B�B	C�B	C�B	D�B	F�B	H�B	J�B	M�B	O�B	P�B	Q�B	R�B	R�B	S�B	T�B	T�B	VB	YB	[#B	[#B	\)B	`BB	aHB	bNB	e`B	ffB	gmB	iyB	jB	k�B	n�B	o�B	o�B	p�B	p�B	q�B	q�B	t�B	y�B	{�B	{�B	|�B	}�B	�B	�B	�%B	�1B	�=B	�=B	�=B	�JB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	��B	�sB	��B
	7B
�B
 �B
+B
0!B
7LB
A�B
I�B
P�B
VB
\)B
`BB
dZB
hsB
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B72B7.B7,B7.B7.B7,B6)B6)B98B96B:@B:=B:@B:@B:@B;CB<MB=TB=TB=TB>[B>XB>XB?]B>UB@eBAlBH�BN�BJ�B>YB-�B*�B �BUB$BB�B�B��B��B��B��B��B��B�bB�@B��B�[B�rBz�BpBW�BBmB*�B�BVB7B
B�B��B��B�B�PB��BͮB�?B��B�tB�?B�B�BiQB]BS�BM�B81B$�BxBCB,B
B�B
��B
��B
�JB
ȐB
��B
oB
j_B
�MB
�dB
��B
��B
�iB
�3B
x�B
Q�B
1	B
�B

B	��B	�B	��B	�VB	�GB	��B	�nB	�mB	�sB	�_B	�B	��B	˨B	�B	�ZB	�FB	�@B	�9B	�/B	�B	�B	��B	��B	��B	�WB	�7B	�,B	�	B	~�B	t�B	nzB	hXB	c9B	Z B	S�B	O�B	L�B	J�B	H�B	D�B	AmB	:CB	+�B	"�B	�B	<B	(B	>B	/B	0B	B	�B��B��B��B�wB�OB�HB�TB�ZB�%B��B˱BȟBŋB�wB�yB�yB�qB�ZB�OB�FB�EB�QB�_B�XB�QB�GB�/B�B�	B� B��B��B��B��B��B��B�~B�oB�]B�MB�3B�%B�B�B�B�B��B��B~�B|�B{�Bz�Bv�Bt�Br�Bp�Bo�Bo�Bn�BlvBkoBieBfPBdCBc>Ba3B^!BZ	BU�BT�BR�BP�BN�BL�BJ�BG�BE�BAuB>dB<VB;OB;PB;NB;PB:GB9EB8>B62B5(B3 B3B3 B1�B/�B/B-�B,�B+�B)�B(�B&�B%�B$�B&�B%�B$�B"�B#�B$�B$�B$�B#�B"�B#�B#�B#�B#�B#�B$�B$�B$�B%�B%�B(�B(�B(�B'�B%�B&�B(�B'�B)�B)�B)�B+�B/B1B5)B>]BE�BI�BG�BD�BAqB@iBApBD�BBvBApBBwBBvBBwBAoB@iB@jB@jBAoBC}BBuBF�BG�BF�BH�BJ�BN�BO�BQ�BR�BU�BU�BV�BX�B[
B`'Bb3BeEBgQBi\BlnBp�Bp�Br�Bv�Bx�Bx�Bz�B~�B��B��B�B�3B�6B�DB�EB�WB�UB�oB�|B�~B�|B�|B�{B�}B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�7B�KB�iBƆBəBʠBϿB��B��B��B�B�B�B�-B�7B�JB�XB�{B�B��B��B��B��B��B	�B	�B	B	B	B	#B	2B	CB	YB	cB	bB	iB	gB	mB	nB	xB	�B	�B	�B	 �B	'�B	-�B	4B	6B	8-B	>NB	@_B	BiB	CqB	CpB	DxB	F�B	H�B	J�B	M�B	O�B	P�B	Q�B	R�B	R�B	S�B	T�B	T�B	U�B	X�B	Z�B	Z�B	\B	`B	a#B	b(B	e7B	f@B	gEB	iSB	jWB	k^B	nqB	oxB	owB	p{B	p|B	q�B	q�B	t�B	y�B	{�B	{�B	|�B	}�B	��B	��B	��B	�	B	�B	�B	�B	�"B	�?B	�SB	�cB	�cB	�dB	�cB	�oB	�qB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�HB	��B
		B
ZB
 �B
*�B
/�B
7B
A[B
I�B
P�B
U�B
[�B
`B
d,B
hFB
mbB
qB
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160309201639    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160309201639  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160309201639  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                