CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-10-22T06:42:09Z creation;2021-10-22T06:42:11Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̀   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211022064209  20211022065221  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @ٜ%
���1   @ٜ'8�9@3�hr�!�dG333331   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D��3D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D���D�<�D�|�D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�3D�C3DԀ D�� D�  D�@ DՀ D��3D�3D�@ D�|�Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�A{A&{AF{Ag�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B��\B�B�B���B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCG�CG�CaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBz�CDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbG�CdaHCfaHChaHCjaHClaHCnz�CpaHCraHCtaHCvaHCxaHCzz�C|aHC~aHC�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�=qC�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�=qC�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�=qC�=qC�0�C�0�C�0�C�0�C�0�C�0�C�#�C�#�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�#�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RD�D��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#�D#�RD$RD$��D%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ��DRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY��DZ�DZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh��DiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDr�Dr�RDs�Ds�RDtRDt�RDuRDu�RDvRDv�RDwRDw�RDxRDx�RDyRDy�RDzRDz�RD{RD{�RD|RD|�RD}RD}�RD~RD~�RDRD�RD�)D�L)D��)D��)D�\D�O\D��)D��)D�)D�L)D��)D��)D�)D�L)D���D���D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D���D��)D�)D�L)D��\D��)D�)D�L)D��\D��)D�)D�O\D��)D��)D�)D�L)D��)D��)D�)D�L)D���D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�O\D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��\D��)D�)D�L)D��)D��)D�)D�L)D��)D��\D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�\D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�O\D��)D���D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��\D��\D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��\D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D���D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��\D��)D�)D�L)D)D��)D�)D�L)DÌ)D��)D�)D�L)DČ)D��)D�)D�L)DŌ)D��)D�)D�L)Dƌ)D��)D�)D�L)Dǌ)D��)D�)D�L)DȌ)D��)D�)D�L)DɌ)D��\D�)D�L)Dʌ)D��)D�)D�L)Dˌ)D��\D�)D�L)Ď)D��)D��D�H�D͈�D��)D�)D�H�DΌ)D��)D�)D�L)Dό)D��)D�)D�L)DЌ)D��)D�)D�L)Dь)D��)D�)D�L)D҈�D��)D�)D�L)Dӈ�D��)D�\D�O\DԌ)D��)D�)D�L)DՌ)D��\D�\D�L)Dֈ�D���D�)D�L)D׌)D��)D�)D�L)D،)D��)D�)D�L)Dٌ)D��)D�)D�L)Dڌ)D��)D�)D�L)Dی)D��)D�)D�L)D܌)D��)D�)D�L)D݌)D��)D�)D�L)Dތ)D��)D�)D�L)Dߌ)D��)D��D�L)D��)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�\D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D��)D��)D�)D�L)D�)D��\D�)D�H�D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�O\D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A��
A��/A��TA��TA��HA��HA��TA��TA��`A��mA��mA��mA��yA��mA�ĜA�n�A�ȴA�t�A�A�A���AնFA�z�A�-A���Aԡ�A�/AӃA��
A��/A�S�A̴9A�dZAʾwA�oA�\)A��Aã�A�Q�A�-A�v�A���A�5?A��A�-A��A��A�l�A�O�A�A�A�Q�A�G�A���A�{A�x�A�;dA���A�"�A��^A�+A�z�A���A�33A��A���A�ƨA���A��A�\)A���A�(�A���A��A�&�A��A��A�"�A���A��hA�G�A��7A��A�33A��wA�+A�33A�ƨA��A���A�JA�`BA�G�A�1A�5?A���A�1'A�A��uA���A~VAy�Awp�AuhsAt=qAp��Am��Aj�RAgdZAfĜAdjAdbAb��A`A�A\��AZ�\AX-AV�ASARv�AO�FAMC�AK�AHA�AG��AF��ADbABI�A?�A?+A>��A=A;x�A9\)A7A6��A6�RA5`BA3��A0ffA.VA-XA,��A)ƨA'
=A%�#A$ffA#&�A!�^A ��A 5?A��A��A��At�A�9A�^AK�A��A�-A �AG�A?}A�RA~�A�A�DA�7A/A/A$�AO�A�A�
A�A~�A$�A
I�A	��A	33A�uA`BA`BA1'A|�A�A��A �9@�p�@���@�I�@�t�@�+@�9X@�ff@� �@�;d@��@�p�@�j@�"�@���@���@� �@���@���@�@�v�@� �@���@ޟ�@�~�@�ff@��@ܣ�@�@���@��@��@ج@թ�@�9X@���@�1'@���@�%@��;@�ȴ@ə�@ț�@�ƨ@�5?@��@�bN@�1'@�C�@��y@�E�@��-@���@��@���@��@��@�bN@�ff@��#@�`B@��@��@�|�@��@��@���@�K�@�@���@�9X@�dZ@���@�Z@�dZ@�O�@�9X@�v�@��@��9@�Z@�  @��@�E�@��T@��^@���@��@��@���@��@���@�|�@���@���@�\)@��@�
=@��-@�O�@���@���@�z�@�  @��m@��F@��@�o@��H@�^5@�5?@��@�J@�J@�@���@��-@�X@�%@��u@�(�@��;@��
@��F@��@���@�l�@�dZ@�@���@���@���@���@�M�@�x�@�O�@���@���@�(�@���@��@�|�@�t�@�t�@�|�@��@�t�@�S�@�;d@�+@�@���@�J@��-@���@���@�p�@�7L@��@��@��9@���@��9@��@�r�@�(�@��@���@�K�@�C�@�S�@��@�+@�Z@�@��h@���@��@���@�`B@�hs@�`B@�p�@�X@�x�@��@�O�@�V@��u@�b@���@�  @���@��;@��
@�ƨ@�t�@�33@��y@���@��!@�M�@��T@���@�?}@���@��j@�9X@�  @���@���@�t�@�
=@�ȴ@�~�@�n�@�@���@�O�@�%@��@�  @��P@�C�@�
=@��H@��!@���@�M�@�$�@��-@�G�@���@��@��@�z�@�9X@�1@��@�ƨ@�t�@�+@�o@�
=@��H@�M�@��#@��7@��@��@��@�x�@�x�@�p�@�p�@�7L@��`@��`@���@��j@���@���@��@�9X@�  @��w@�S�@��@�ȴ@�$�@���@��-@�x�@�&�@��`@�bN@�9X@��@��@��
@���@��@�S�@�
=@���@�E�@��@���@�hs@��@���@�z�@�1'@�ƨ@��@���@��P@�|�@�dZ@�;d@�o@��H@���@��!@��+@�E�@�J@�@��7@�O�@���@���@�j@�1'@�1@l�@~��@~�y@~�+@~E�@~@}�-@}O�@|�/@|�j@|�@|�@|�D@|Z@|Z@|Z@|9X@{�@z��@zn�@z^5@z=q@z�@y�#@yhs@x��@x��@x�9@w�@w�P@v�y@v��@v�+@vV@v5?@v$�@u�@u�-@up�@u/@t��@s�m@s�@so@r�\@q��@q&�@p�u@pbN@p1'@o;d@nv�@n@m�-@mO�@l�@l�@k��@k�@kS�@ko@j�H@j~�@i��@h�9@hQ�@g�;@g�@g��@g;d@f�R@fff@e`B@d��@d��@dZ@cƨ@c��@ct�@co@b�H@b~�@b^5@b�@a��@`��@`r�@`b@_�w@_\)@_
=@^E�@]p�@]�@]V@\��@\�@\�/@\Z@[�F@[C�@Z�H@Z~�@Y%@Xr�@X1'@W�P@W�@Vȴ@V{@U�@U�-@U�h@U`B@T��@T�j@T��@TZ@S�
@S�@S"�@R�@R�!@R�\@RM�@R-@Q��@Qx�@PĜ@P�@PbN@P1'@P1'@Pb@O�w@Ol�@O�@N��@N�+@Nff@N{@M��@M�h@M/@M�@L��@LI�@K��@KS�@J�\@I�#@I�7@I7L@I&�@I�@I%@H�9@H�@HA�@G�w@G+@F�R@Fv�@FV@F{@EO�@D�/@D�@D9X@C��@C�F@Ct�@CC�@C33@B��@A��@A��@A�^@A�7@Ahs@AG�@A&�@@��@@��@@Q�@@1'@?�;@?�P@?\)@>�y@>�R@>��@>�+@>E�@=��@=�h@=V@<�j@<�D@<I�@<1@;��@;S�@;C�@;"�@:�H@:�\@9��@9��@9X@97L@9%@8��@7�;@7l�@7+@6��@5�@5��@4�@4�D@4I�@4�@3��@3��@3"�@2�!@2^5@2M�@2=q@2�@1�#@1��@1�7@1x�@1G�@1�@1%@0�9@0r�@0r�@/�;@/�P@/�@.V@-�-@-`B@-�@,j@,Z@,I�@,�@+ƨ@+��@+C�@*�\@*-@)��@)G�@(��@(��@(�u@(bN@(1'@'��@';d@&�y@&�R@&��@&v�@&E�@%�@%@%�@%p�@%�@%�@%O�@%?}@$��@$Z@$(�@#��@#��@#dZ@#S�@#S�@#"�@"�!@"~�@"�@!hs@!&�@ �`@ �9@ bN@ b@��@|�@K�@�@��@�@�R@��@E�@$�@�@�-@��@��@��@��@��@�-@��@�@O�@�@��@z�@Z@(�@��@��@t�@S�@@��@��@n�@M�@M�@�@�@��@�^@��@G�@%@Ĝ@r�@  @�@�w@�P@|�@l�@�@�@v�@ff@ff@5?@�@��@p�@V@V@��@��@�/@�D@Z@I�@�@��@ƨ@�@C�@"�@�@��@��@n�@M�@M�@=q@�@��@��@�7@�7@hs@X@G�@�@%@��@��@��@%@��@�`@��@�@�@r�@Q�@A�@b@�@��@|�@\)@�@ȴ@v�@V@$�@�T@@��@�h@p�@O�@V@�/@��@I�@9X@(�@(�@�@�@1@1@��@�
@�F@dZ@o@
�H@
��@
��@
~�@
=q@
�@
�@	�@	�#@	��@	�^@	��@	�7@	hs@	hs@	X@	G�@	&�@	�@��@��@r�@ �@�;@�w@�@��@\)@K�@;d@+@��@ȴ@v�@V@E�@{@�T@�-@�@`B@`B@?}@��@�/@��@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A��
A��/A��TA��TA��HA��HA��TA��TA��`A��mA��mA��mA��yA��mA�ĜA�n�A�ȴA�t�A�A�A���AնFA�z�A�-A���Aԡ�A�/AӃA��
A��/A�S�A̴9A�dZAʾwA�oA�\)A��Aã�A�Q�A�-A�v�A���A�5?A��A�-A��A��A�l�A�O�A�A�A�Q�A�G�A���A�{A�x�A�;dA���A�"�A��^A�+A�z�A���A�33A��A���A�ƨA���A��A�\)A���A�(�A���A��A�&�A��A��A�"�A���A��hA�G�A��7A��A�33A��wA�+A�33A�ƨA��A���A�JA�`BA�G�A�1A�5?A���A�1'A�A��uA���A~VAy�Awp�AuhsAt=qAp��Am��Aj�RAgdZAfĜAdjAdbAb��A`A�A\��AZ�\AX-AV�ASARv�AO�FAMC�AK�AHA�AG��AF��ADbABI�A?�A?+A>��A=A;x�A9\)A7A6��A6�RA5`BA3��A0ffA.VA-XA,��A)ƨA'
=A%�#A$ffA#&�A!�^A ��A 5?A��A��A��At�A�9A�^AK�A��A�-A �AG�A?}A�RA~�A�A�DA�7A/A/A$�AO�A�A�
A�A~�A$�A
I�A	��A	33A�uA`BA`BA1'A|�A�A��A �9@�p�@���@�I�@�t�@�+@�9X@�ff@� �@�;d@��@�p�@�j@�"�@���@���@� �@���@���@�@�v�@� �@���@ޟ�@�~�@�ff@��@ܣ�@�@���@��@��@ج@թ�@�9X@���@�1'@���@�%@��;@�ȴ@ə�@ț�@�ƨ@�5?@��@�bN@�1'@�C�@��y@�E�@��-@���@��@���@��@��@�bN@�ff@��#@�`B@��@��@�|�@��@��@���@�K�@�@���@�9X@�dZ@���@�Z@�dZ@�O�@�9X@�v�@��@��9@�Z@�  @��@�E�@��T@��^@���@��@��@���@��@���@�|�@���@���@�\)@��@�
=@��-@�O�@���@���@�z�@�  @��m@��F@��@�o@��H@�^5@�5?@��@�J@�J@�@���@��-@�X@�%@��u@�(�@��;@��
@��F@��@���@�l�@�dZ@�@���@���@���@���@�M�@�x�@�O�@���@���@�(�@���@��@�|�@�t�@�t�@�|�@��@�t�@�S�@�;d@�+@�@���@�J@��-@���@���@�p�@�7L@��@��@��9@���@��9@��@�r�@�(�@��@���@�K�@�C�@�S�@��@�+@�Z@�@��h@���@��@���@�`B@�hs@�`B@�p�@�X@�x�@��@�O�@�V@��u@�b@���@�  @���@��;@��
@�ƨ@�t�@�33@��y@���@��!@�M�@��T@���@�?}@���@��j@�9X@�  @���@���@�t�@�
=@�ȴ@�~�@�n�@�@���@�O�@�%@��@�  @��P@�C�@�
=@��H@��!@���@�M�@�$�@��-@�G�@���@��@��@�z�@�9X@�1@��@�ƨ@�t�@�+@�o@�
=@��H@�M�@��#@��7@��@��@��@�x�@�x�@�p�@�p�@�7L@��`@��`@���@��j@���@���@��@�9X@�  @��w@�S�@��@�ȴ@�$�@���@��-@�x�@�&�@��`@�bN@�9X@��@��@��
@���@��@�S�@�
=@���@�E�@��@���@�hs@��@���@�z�@�1'@�ƨ@��@���@��P@�|�@�dZ@�;d@�o@��H@���@��!@��+@�E�@�J@�@��7@�O�@���@���@�j@�1'@�1@l�@~��@~�y@~�+@~E�@~@}�-@}O�@|�/@|�j@|�@|�@|�D@|Z@|Z@|Z@|9X@{�@z��@zn�@z^5@z=q@z�@y�#@yhs@x��@x��@x�9@w�@w�P@v�y@v��@v�+@vV@v5?@v$�@u�@u�-@up�@u/@t��@s�m@s�@so@r�\@q��@q&�@p�u@pbN@p1'@o;d@nv�@n@m�-@mO�@l�@l�@k��@k�@kS�@ko@j�H@j~�@i��@h�9@hQ�@g�;@g�@g��@g;d@f�R@fff@e`B@d��@d��@dZ@cƨ@c��@ct�@co@b�H@b~�@b^5@b�@a��@`��@`r�@`b@_�w@_\)@_
=@^E�@]p�@]�@]V@\��@\�@\�/@\Z@[�F@[C�@Z�H@Z~�@Y%@Xr�@X1'@W�P@W�@Vȴ@V{@U�@U�-@U�h@U`B@T��@T�j@T��@TZ@S�
@S�@S"�@R�@R�!@R�\@RM�@R-@Q��@Qx�@PĜ@P�@PbN@P1'@P1'@Pb@O�w@Ol�@O�@N��@N�+@Nff@N{@M��@M�h@M/@M�@L��@LI�@K��@KS�@J�\@I�#@I�7@I7L@I&�@I�@I%@H�9@H�@HA�@G�w@G+@F�R@Fv�@FV@F{@EO�@D�/@D�@D9X@C��@C�F@Ct�@CC�@C33@B��@A��@A��@A�^@A�7@Ahs@AG�@A&�@@��@@��@@Q�@@1'@?�;@?�P@?\)@>�y@>�R@>��@>�+@>E�@=��@=�h@=V@<�j@<�D@<I�@<1@;��@;S�@;C�@;"�@:�H@:�\@9��@9��@9X@97L@9%@8��@7�;@7l�@7+@6��@5�@5��@4�@4�D@4I�@4�@3��@3��@3"�@2�!@2^5@2M�@2=q@2�@1�#@1��@1�7@1x�@1G�@1�@1%@0�9@0r�@0r�@/�;@/�P@/�@.V@-�-@-`B@-�@,j@,Z@,I�@,�@+ƨ@+��@+C�@*�\@*-@)��@)G�@(��@(��@(�u@(bN@(1'@'��@';d@&�y@&�R@&��@&v�@&E�@%�@%@%�@%p�@%�@%�@%O�@%?}@$��@$Z@$(�@#��@#��@#dZ@#S�@#S�@#"�@"�!@"~�@"�@!hs@!&�@ �`@ �9@ bN@ b@��@|�@K�@�@��@�@�R@��@E�@$�@�@�-@��@��@��@��@��@�-@��@�@O�@�@��@z�@Z@(�@��@��@t�@S�@@��@��@n�@M�@M�@�@�@��@�^@��@G�@%@Ĝ@r�@  @�@�w@�P@|�@l�@�@�@v�@ff@ff@5?@�@��@p�@V@V@��@��@�/@�D@Z@I�@�@��@ƨ@�@C�@"�@�@��@��@n�@M�@M�@=q@�@��@��@�7@�7@hs@X@G�@�@%@��@��@��@%@��@�`@��@�@�@r�@Q�@A�@b@�@��@|�@\)@�@ȴ@v�@V@$�@�T@@��@�h@p�@O�@V@�/@��@I�@9X@(�@(�@�@�@1@1@��@�
@�F@dZ@o@
�H@
��@
��@
~�@
=q@
�@
�@	�@	�#@	��@	�^@	��@	�7@	hs@	hs@	X@	G�@	&�@	�@��@��@r�@ �@�;@�w@�@��@\)@K�@;d@+@��@ȴ@v�@V@E�@{@�T@�-@�@`B@`B@?}@��@�/@��@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BO�B_;B~�B�{B��B��B��B��B�B�!B�3B�3B�RB��B��B��B�NB�yB�B��BBbB�B-B-B-B>wBO�BO�BN�BL�BI�BB�BG�B@�B8RB49B-B%�B"�B$�B8RBD�BP�BXBffB]/B@�B2-B"�B
=B%BoBoB\B��B�`B�B��BɺB��B�XB�B��B�oB�7Bv�BgmBT�BD�B2-B"�B�BJB
�`B
��B
�-B
��B
�{B
�\B
�B
x�B
e`B
S�B
9XB
�B
+B	��B	��B	�BB	��B	�wB	�B	��B	��B	��B	�hB	�1B	u�B	iyB	^5B	\)B	O�B	F�B	?}B	0!B	0!B	�B	�B	�B	�B	PB	1B	B	B��B��B��B��B��B��B�B�yB�)B��B��BȴB�}B�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�JB�PB�VB�7B�B�B�B�B�B�1B�B�%B�B�%B�%B�B�B�B� B� B� B� B}�B}�B~�B}�Bz�Bs�Bo�Bk�Bk�Bk�BjBhsBiyBhsBhsBiyBhsBiyBhsBhsBiyBm�Bn�Bo�Bo�Bp�Bo�Bp�Bp�Bp�Bo�Bq�Bv�Bx�Bw�By�B�B�B�7B�JB�JB�PB�VB�bB��B��B��B��B��B�B�B�B�3B�FB�LB�FB�RB�dBƨB��B�BB�NB�5B�5B�;B�HB�BB�BB�TB�fB�yB�B�mB�mB�sB�yB�B�sB�mB�NB�;B�;B�HB�NB�ZB�`B�yB�B��B��B��B��B	B	B	B	1B	DB	bB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	'�B	+B	-B	33B	6FB	8RB	8RB	8RB	8RB	8RB	7LB	9XB	;dB	B�B	F�B	G�B	G�B	I�B	I�B	K�B	S�B	W
B	^5B	`BB	aHB	aHB	aHB	aHB	cTB	ffB	iyB	k�B	q�B	s�B	s�B	s�B	t�B	t�B	t�B	u�B	v�B	x�B	y�B	y�B	y�B	}�B	�B	�%B	�%B	�+B	�1B	�7B	�=B	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�qB	��B	ƨB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�BB	�TB	�TB	�`B	�fB	�fB	�fB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
VB
\B
VB
VB
VB
\B
\B
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
=qB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BO�B_;B~�B�{B��B��B��B��B�B�!B�3B�3B�RB��B��B��B�NB�yB�B��BBbB�B-B-B-B>wBO�BO�BN�BL�BI�BB�BG�B@�B8RB49B-B%�B"�B$�B8RBD�BP�BXBffB]/B@�B2-B"�B
=B%BoBoB\B��B�`B�B��BɺB��B�XB�B��B�oB�7Bv�BgmBT�BD�B2-B"�B�BJB
�`B
��B
�-B
��B
�{B
�\B
�B
x�B
e`B
S�B
9XB
�B
+B	��B	��B	�BB	��B	�wB	�B	��B	��B	��B	�hB	�1B	u�B	iyB	^5B	\)B	O�B	F�B	?}B	0!B	0!B	�B	�B	�B	�B	PB	1B	B	B��B��B��B��B��B��B�B�yB�)B��B��BȴB�}B�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�JB�PB�VB�7B�B�B�B�B�B�1B�B�%B�B�%B�%B�B�B�B� B� B� B� B}�B}�B~�B}�Bz�Bs�Bo�Bk�Bk�Bk�BjBhsBiyBhsBhsBiyBhsBiyBhsBhsBiyBm�Bn�Bo�Bo�Bp�Bo�Bp�Bp�Bp�Bo�Bq�Bv�Bx�Bw�By�B�B�B�7B�JB�JB�PB�VB�bB��B��B��B��B��B�B�B�B�3B�FB�LB�FB�RB�dBƨB��B�BB�NB�5B�5B�;B�HB�BB�BB�TB�fB�yB�B�mB�mB�sB�yB�B�sB�mB�NB�;B�;B�HB�NB�ZB�`B�yB�B��B��B��B��B	B	B	B	1B	DB	bB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	'�B	+B	-B	33B	6FB	8RB	8RB	8RB	8RB	8RB	7LB	9XB	;dB	B�B	F�B	G�B	G�B	I�B	I�B	K�B	S�B	W
B	^5B	`BB	aHB	aHB	aHB	aHB	cTB	ffB	iyB	k�B	q�B	s�B	s�B	s�B	t�B	t�B	t�B	u�B	v�B	x�B	y�B	y�B	y�B	}�B	�B	�%B	�%B	�+B	�1B	�7B	�=B	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�qB	��B	ƨB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�BB	�TB	�TB	�`B	�fB	�fB	�fB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
VB
\B
VB
VB
VB
\B
\B
VB
VB
\B
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
=qB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20211022154037  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20211022064209  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20211022064209  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20211022064209  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20211022064210  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20211022064210  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20211022064210  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20211022064210  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20211022064210  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20211022064211                      G�O�G�O�G�O�                JA  ARUP                                                                        20211022065221                      G�O�G�O�G�O�                