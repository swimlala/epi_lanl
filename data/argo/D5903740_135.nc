CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-08T20:16:22Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151208201622  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_135                   2C  D   APEX                            5374                            041511                          846 @ׄ�!��a1   @ׄ���(�@:��t�j�c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Dr��Ds� Dt  Dy�fD�fD�FfD���D��fD���D�FfD�c3D��3D�fD�\�D�� D�ɚD��3D�6fDړ3D��fD��D�6fD�Y�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B���B���B�B��\B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCG�CaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDS�DS�RDT�DT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr��Ds�Ds�RDtRDy޸D��D�R�D���D�D��D�R�D�o\D��\D�"�D�h�D��)D���D��\D�B�Dڟ\D�ҏD���D�B�D�e�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�S�A�\)A�\)A�ZA�VA�M�A�\)A�dZA�dZA�ffA�dZA�dZA�ffA�dZA�ffA�jA�jA�hsA�jA�jA�jA�l�A�l�A�l�A�l�A�n�A�l�A�jA�bNA�bNA�ZA�ZA�\)A�A�A� �A��/A�\)A�{A�  A���A���A�\)A�A��PA���A�VA�~�A��yA� �A�oA��A�K�A�v�A�O�A�|�A���A�A��yA�I�A�JA�ffA�oA��A���A�I�A��!A�bA�bNA�XA���A��A��-A�VA�p�A���A���A���A��/A���A�|�A�I�A���A���A�ȴA��A��yA���A�JA�;dA��A�r�A�l�A��A�ĜA��!A~Q�A}x�A|5?AzE�Aw�AvE�At��Aq�Ap�uAo�Ao�PAn��Am�Al5?AkO�Aj��Aj�+Ai��Ah�+Ag�Af�RAeAe�Ad�Ad�RAd��Adv�Ac��AbZA`�uA^�RA]�hA\ĜA[�;AZ�uAYl�AY�AYhsAW��AV-AU33ATM�AS�AR�ARz�AQ�AQ��AQhsAO�PAN�RAM��AL��AKx�AJ��AJ(�AI��AIp�AH��AH  AG"�AE�7ADn�AC��AC?}ACABbA?��A>r�A="�A;�PA;;dA;"�A:��A:A�A:1A8��A8�+A8M�A6�9A4��A3+A2(�A1��A1?}A1VA0�A0��A0Q�A//A.^5A-`BA+�
A*�9A*E�A)��A(n�A'C�A&�A&E�A%hsA%oA$M�A#l�A"�yA!A!�A �\A 5?A|�A+A��A  A|�AA��A�
Al�Al�AK�A"�AA��AM�A/AjAK�A�yAn�A{A�FAK�A�mA$�A�An�A5?A��AXA��A�A(�A|�A;dA
{A�AffAoAn�A�TA�HA ��A A�@��!@�@�j@�S�@�{@�`B@��D@�@�G�@�Q�@�|�@���@��@�@�K�@�^5@�%@��;@�\)@�o@�!@�O�@� �@䛦@�p�@�/@���@�ȴ@���@�&�@���@�l�@�-@�%@�(�@�S�@ա�@�^5@мj@�o@�5?@˾w@ʸR@ʸR@ʧ�@��@��@Ɨ�@�X@ēu@���@Ý�@���@�%@��
@�ff@�{@���@�bN@���@�dZ@��R@�=q@���@���@�Q�@�1@���@���@�{@���@�1'@��@�t�@�t�@���@��\@���@��@��m@�l�@�S�@�"�@��@�G�@���@�\)@��y@�V@��@���@���@�(�@���@�+@�-@�?}@�V@�Ĝ@��u@�C�@�hs@�33@��@��R@��-@�hs@�&�@��/@��m@�33@���@�v�@���@�E�@��#@���@�@�G�@���@���@�r�@��@��@��@���@�^5@���@���@��@�G�@���@�Ĝ@���@�r�@�I�@���@���@�S�@�o@���@�{@�O�@�&�@��`@���@��@���@�A�@��
@�t�@�;d@�@��H@���@���@���@�~�@�^5@�{@���@�O�@��/@���@�j@�Z@�1'@��
@�K�@�@���@��H@��H@��y@��@�ȴ@��!@��!@��!@��!@��!@��R@�ȴ@��@��!@�$�@��#@��7@�hs@�hs@�?}@�%@�Ĝ@��j@��9@��u@�Q�@�1@��w@��F@���@�|�@�K�@�33@�"�@�@���@�$�@���@���@��h@�x�@�`B@�X@�G�@�/@�V@��@���@��9@���@��u@�z�@�j@�A�@� �@� �@�b@��m@���@�l�@�+@��y@��\@�ff@�M�@��@�@��@~�@u�@n@c��@]�-@U�@NV@H1'@Co@=?}@4(�@,I�@(1'@"n�@�@��@�;@"�@�;@
^5@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�M�A�S�A�\)A�\)A�ZA�VA�M�A�\)A�dZA�dZA�ffA�dZA�dZA�ffA�dZA�ffA�jA�jA�hsA�jA�jA�jA�l�A�l�A�l�A�l�A�n�A�l�A�jA�bNA�bNA�ZA�ZA�\)A�A�A� �A��/A�\)A�{A�  A���A���A�\)A�A��PA���A�VA�~�A��yA� �A�oA��A�K�A�v�A�O�A�|�A���A�A��yA�I�A�JA�ffA�oA��A���A�I�A��!A�bA�bNA�XA���A��A��-A�VA�p�A���A���A���A��/A���A�|�A�I�A���A���A�ȴA��A��yA���A�JA�;dA��A�r�A�l�A��A�ĜA��!A~Q�A}x�A|5?AzE�Aw�AvE�At��Aq�Ap�uAo�Ao�PAn��Am�Al5?AkO�Aj��Aj�+Ai��Ah�+Ag�Af�RAeAe�Ad�Ad�RAd��Adv�Ac��AbZA`�uA^�RA]�hA\ĜA[�;AZ�uAYl�AY�AYhsAW��AV-AU33ATM�AS�AR�ARz�AQ�AQ��AQhsAO�PAN�RAM��AL��AKx�AJ��AJ(�AI��AIp�AH��AH  AG"�AE�7ADn�AC��AC?}ACABbA?��A>r�A="�A;�PA;;dA;"�A:��A:A�A:1A8��A8�+A8M�A6�9A4��A3+A2(�A1��A1?}A1VA0�A0��A0Q�A//A.^5A-`BA+�
A*�9A*E�A)��A(n�A'C�A&�A&E�A%hsA%oA$M�A#l�A"�yA!A!�A �\A 5?A|�A+A��A  A|�AA��A�
Al�Al�AK�A"�AA��AM�A/AjAK�A�yAn�A{A�FAK�A�mA$�A�An�A5?A��AXA��A�A(�A|�A;dA
{A�AffAoAn�A�TA�HA ��A A�@��!@�@�j@�S�@�{@�`B@��D@�@�G�@�Q�@�|�@���@��@�@�K�@�^5@�%@��;@�\)@�o@�!@�O�@� �@䛦@�p�@�/@���@�ȴ@���@�&�@���@�l�@�-@�%@�(�@�S�@ա�@�^5@мj@�o@�5?@˾w@ʸR@ʸR@ʧ�@��@��@Ɨ�@�X@ēu@���@Ý�@���@�%@��
@�ff@�{@���@�bN@���@�dZ@��R@�=q@���@���@�Q�@�1@���@���@�{@���@�1'@��@�t�@�t�@���@��\@���@��@��m@�l�@�S�@�"�@��@�G�@���@�\)@��y@�V@��@���@���@�(�@���@�+@�-@�?}@�V@�Ĝ@��u@�C�@�hs@�33@��@��R@��-@�hs@�&�@��/@��m@�33@���@�v�@���@�E�@��#@���@�@�G�@���@���@�r�@��@��@��@���@�^5@���@���@��@�G�@���@�Ĝ@���@�r�@�I�@���@���@�S�@�o@���@�{@�O�@�&�@��`@���@��@���@�A�@��
@�t�@�;d@�@��H@���@���@���@�~�@�^5@�{@���@�O�@��/@���@�j@�Z@�1'@��
@�K�@�@���@��H@��H@��y@��@�ȴ@��!@��!@��!@��!@��!@��R@�ȴ@��@��!@�$�@��#@��7@�hs@�hs@�?}@�%@�Ĝ@��j@��9@��u@�Q�@�1@��w@��F@���@�|�@�K�@�33@�"�@�@���@�$�@���@���@��h@�x�@�`B@�X@�G�@�/@�V@��@���@��9@���@��u@�z�@�j@�A�@� �@� �@�b@��m@���@�l�@�+@��y@��\@�ff@�M�@��@�@��@~�@u�@n@c��@]�-@U�@NV@H1'@Co@=?}@4(�@,I�@(1'@"n�@�@��@�;@"�@�;@
^5@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B_;B_;B_;B^5B_;B_;B_;B^5B^5B^5B^5B^5B^5B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B^5B^5B^5B^5B^5B]/B]/B]/B[#BZBVBP�BN�BD�B49B�B��B�`B��BDB\BVBDB1BBBB��B�ZB��B�^B��B�DB�Bm�B]/BYBJ�BC�B,B�B+B��B�sB�/B��BƨB�B��B�oB�Bs�BgmBcTB`BB[#BR�BE�B49B#�B�BuB	7B
��B
�yB
��B
�jB
�LB
�!B
��B
�%B
}�B
s�B
ffB
P�B
I�B
=qB
)�B
 �B
�B
�B
\B
B	��B	��B	�B	�B	�B	�TB	�)B	��B	��B	ɺB	ǮB	ƨB	ŢB	B	�qB	�'B	��B	��B	�oB	�PB	�%B	}�B	u�B	~�B	�B	w�B	jB	aHB	]/B	]/B	[#B	ZB	VB	S�B	P�B	J�B	H�B	E�B	A�B	=qB	<jB	:^B	:^B	9XB	9XB	8RB	49B	,B	'�B	$�B	"�B	 �B	�B	PB	B��B��B��B��B��B�B�B�B�B�mB�HB�B��B��B��B��B��B��B��BɺBŢBB�qB�?B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�DB�7B�1B�%B�B�B�B�B�B� B� B~�B|�Bz�Bw�Bt�Br�Br�Bq�Bo�Bm�BjBdZBbNBaHB`BB_;B^5B[#BXBVBS�BR�BP�BK�BI�BE�BD�BB�B@�B<jB:^B9XB8RB7LB6FB5?B49B49B33B2-B1'B0!B/B-B,B,B+B+B+B,B,B+B)�B(�B&�B$�B%�B$�B#�B%�B%�B%�B$�B$�B$�B$�B#�B"�B �B!�B!�B"�B"�B%�B'�B'�B'�B(�B)�B-B-B.B0!B0!B1'B5?B6FB9XB9XB;dB<jB=qB=qB>wB>wB?}B@�BA�BA�BA�BC�BD�BH�BI�BI�BJ�BI�BJ�BJ�BL�BO�BQ�BS�BS�BS�BS�BXB]/B^5B`BBaHBcTBcTBffBk�Bn�Bs�By�B|�B~�B� B� B�B�\B��B��B��B��B��B��B�B�B�'B�-B�9B�3B�9B�FB�LB�LB�jB�wB�wB�}BBĜBƨB��B��B�)B�/B�/B�;B�HB�NB�TB�`B�fB�mB�B�B�B�B��B��B��B��B��B	  B	  B	B	%B		7B	DB	VB	\B	bB	bB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	)�B	)�B	+B	+B	+B	,B	,B	.B	/B	0!B	0!B	0!B	1'B	49B	6FB	:^B	B�B	E�B	I�B	K�B	K�B	L�B	O�B	Q�B	Q�B	Q�B	R�B	VB	YB	]/B	]/B	^5B	`BB	aHB	bNB	cTB	dZB	ffB	iyB	l�B	m�B	n�B	n�B	o�B	p�B	p�B	r�B	s�B	u�B	v�B	w�B	x�B	x�B	y�B	y�B	z�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�B	�%B	�+B	�1B	�=B	�DB	�JB	��B	�^B	��B	�B	��B
JB
�B
�B
%�B
/B
9XB
C�B
H�B
M�B
P�B
XB
\)B
cTB
ffB
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B_B_B_B_B^B_B_B_B^B^B^B^B^B^B_B_B_B_B_B_B_B_B_B_B_B_B^B^B^B^B^B]B]B]B[BY�BU�BP�BN�BD�B4B�B��B�AB��B%B;B4B#BB�B�B �B��B�7BϽB�:B��B�B��BmnB]	BX�BJ�BCuB+�BuBB��B�PB�
B��BƂB��B��B�KB��Bs�BgHBc/B`B[BR�BE|B4B#�BtBQB	B
��B
�UB
ͮB
�HB
�'B
� B
��B
�B
}�B
s�B
fEB
P�B
I�B
=SB
)�B
 �B
�B
mB
;B
�B	��B	��B	�B	��B	�]B	�4B	�B	��B	νB	ɚB	ǐB	ƌB	ŃB	�rB	�QB	�	B	��B	�zB	�QB	�3B	�B	}�B	u�B	~�B	��B	w�B	jbB	a,B	]B	]B	[B	ZB	U�B	S�B	P�B	J�B	H�B	E�B	AlB	=UB	<PB	:DB	:DB	9=B	9=B	85B	4B	+�B	'�B	$�B	"�B	 �B	wB	6B	B��B��B��B��B��B�B�B�yB�gB�TB�0B�B��B��B��BͻBͺB̷B˯BɤBŉB�zB�XB�'B�B��B��B��B��B�zB�wB��B��B��B�}B�jB�eB�YB�LB�AB�3B�.B�!B�B�B�B��B��B��B��B�B�B~�B|�Bz�Bw�Bt�Br�Br�Bq�Bo�Bm|BjiBdCBb:Ba1B`/B_%B^ B[BW�BU�BS�BR�BP�BK�BI�BE�BD�BByB@oB<TB:IB9CB8=B75B62B5)B4$B4"B3 B2B0�B0B.�B,�B+�B+�B*�B*�B*�B+�B+�B*�B)�B(�B&�B$�B%�B$�B#�B%�B%�B%�B$�B$�B$�B$�B#�B"�B �B!�B!�B"�B"�B%�B'�B'�B'�B(�B)�B,�B,�B-�B0
B0B1B5&B6,B9BB9@B;KB<QB=YB=XB>aB>]B?cB@iBAoBAoBApBCzBD�BH�BI�BI�BJ�BI�BJ�BJ�BL�BO�BQ�BS�BS�BS�BS�BW�B]B^B`$Ba+Bc;Bc9BfJBkiBn{Bs�By�B|�B~�B�B�B�B�>B�cB��B��B��B��B��B��B��B�B�B�B�B�B�'B�*B�,B�JB�XB�VB�^B�qB�~BƅBʟB��B�B�B�B�B�&B�+B�1B�<B�DB�KB�]B�hB�vB��B��B��B��B��B��B��B��B	�B	B		B	"B	3B	8B	@B	@B	EB	LB	PB	]B	oB	|B	�B	�B	�B	�B	 �B	#�B	'�B	)�B	)�B	*�B	*�B	*�B	+�B	+�B	-�B	.�B	/�B	/�B	/�B	1B	4B	6B	:8B	BgB	E{B	I�B	K�B	K�B	L�B	O�B	Q�B	Q�B	Q�B	R�B	U�B	X�B	]B	]B	^B	`B	a B	b%B	c+B	d2B	f<B	iQB	lfB	mjB	nqB	nrB	owB	p}B	pB	r�B	s�B	u�B	v�B	w�B	x�B	x�B	y�B	y�B	z�B	{�B	{�B	|�B	}�B	~�B	�B	��B	��B	��B	�B	�	B	�B	�B	�#B	�rB	�6B	��B	�tB	��B
B
aB
�B
%�B
.�B
9(B
CiB
H�B
M�B
P�B
W�B
[�B
c(B
f;B
l\B
q|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20151208201622    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151208201622  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151208201622  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                