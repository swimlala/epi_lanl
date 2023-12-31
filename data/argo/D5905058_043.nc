CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-02T06:35:13Z creation;2018-03-02T06:35:15Z conversion to V3.1;2019-12-23T06:26:08Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20180302063513  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  I2_0675_043                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�PE�N��1   @�PFH+�@6����C��b�J�L�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�3D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:G�C<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD�D�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4��D5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDuRDu�RDvRDv�RDwRDw�RDxRDx�RDyRDy�RDzRDz�RD{RD{�RD|RD|�RD}RD}�RD~RD~�RDRD�RD�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�H�D���D��)D�)D�L)D��)D��)D�\D�L)D��)D���D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�O\D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D���D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�\D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D��)D��)D�)D�L)D)D��)D�)D�L)DÌ)D��)D�\D�L)DČ)D��)D�)D�L)DŌ)D��)D�)D�L)Dƌ)D��)D�)D�L)Dǌ)D��)D�)D�L)DȌ)D��)D�)D�L)DɌ)D��)D�)D�L)Dʌ)D��)D�)D�L)Dˌ)D��)D�)D�L)Ď)D��)D�)D�L)D͌)D��)D�)D�L)DΌ)D��)D�)D�L)Dό)D��)D�)D�L)DЌ)D��)D�)D�L)Dь)D��)D�)D�L)DҌ)D��)D�)D�L)Dӌ)D��)D�)D�L)DԌ)D��)D��D�L)DՌ)D��)D�)D�L)D֌)D��)D�)D�L)D׌)D��)D�)D�L)D،)D��)D�)D�L)Dٌ)D��)D�)D�L)Dڌ)D��)D�)D�L)Dی)D��)D�)D�L)D܌)D��)D�)D�L)D݌)D��)D�)D�L)Dތ)D��)D�)D�L)Dߌ)D��)D�)D�L)D��)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D��)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D��)D�)D�L)D�)D���D�)D�L)D��)D��)D�\D�5�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��/A��yA��yA��yA��yA��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A��A��#A��!A���A��uA��PA�~�A�jA�\)A�I�A�/A���A���A�p�A�K�A�+A�ȴA��7A�-A��\A�7LA���A��A���A��7A��A��A�ffA�p�A��FA�XA��`A��/A���A�9XA�v�A��-A��jA���A�E�A���A�ffA���A��-A��
A�(�A�{A�$�A�/A���A�\)A��jA�^5A�oA��mA���A��RA�p�A�S�A��7A��FA��DA�jA�G�A�
=A�S�A���A�+A�n�A��FA�O�A���A�&�A��RA�I�A�Q�A�A��yA��mA�
=A{��AyK�Aw�
Av�As��Ar=qAoAm\)Ak/Ag%Ad=qAa�A_�hA]VA[&�AY�;AY��AX~�AU&�ARZAQx�AN��AM�-AL��ALAJ��AG�AE�FAC�AB�ABA�A@��A@��A?��A?G�A>�`A>A�A=��A<��A<(�A9��A8bNA6ĜA5l�A4�!A4bNA3��A3;dA2�A0bNA-�mA,z�A,{A*1A(�RA'�PA&E�A%|�A$E�A"bNA�A|�AVAp�AC�A1'A`BA��A��Ar�A�mAƨA�Az�AVA�uAffA{AƨA��A  A��A
�yA	S�A�jAjAoA�A�A��A$�A ��@�V@��@��
@�dZ@�n�@�J@���@��`@�Z@���@�hs@�\@��/@�z�@�I�@��@��y@�@��`@�(�@�\)@�~�@���@���@�ff@���@�?}@�ƨ@��/@��
@��H@�n�@ݲ-@��@��`@�9X@�M�@ش9@�  @�33@ԛ�@�o@�=q@�-@�?}@�K�@���@��/@�l�@�J@ə�@ɉ7@ǅ@ź^@�O�@��@Ĵ9@�\)@�hs@�b@�;d@�ȴ@��h@��@��;@�l�@�5?@�7L@�j@�A�@�+@��T@��@�Q�@�  @�ƨ@�l�@�;d@���@�=q@�5?@��@���@�`B@�G�@�7L@�Ĝ@��j@�z�@��P@�K�@�K�@��H@�5?@�V@�I�@�A�@�9X@�9X@��@��@���@�V@�J@���@��/@�A�@�1@�dZ@���@�E�@��@��h@��@�z�@�bN@�A�@�1@��
@�t�@���@���@���@�/@���@�Ĝ@��9@���@��D@�A�@��P@�C�@�"�@��y@���@��R@��+@���@�p�@�G�@�7L@�7L@�/@�/@�Ĝ@��;@���@�|�@�;d@�@��@��@��!@�~�@�E�@��-@�&�@���@�Ĝ@��@�b@�ƨ@�\)@���@���@�n�@�E�@�@��T@���@�?}@�&�@��/@���@��D@�z�@�Z@�(�@�b@��m@��@��P@�t�@�S�@�33@��@�~�@���@��^@��@�X@�/@�&�@���@��@��/@��D@�A�@�1'@�(�@� �@��@�b@�1@���@�t�@�C�@��@��@���@�V@�E�@�=q@��@��#@��7@�p�@�`B@�O�@�G�@��@�%@��j@��@���@�r�@�Q�@�Q�@�9X@� �@��@� �@� �@�1@��m@��w@�dZ@�;d@���@���@���@�n�@�J@�@���@���@��7@�G�@�/@��@��@��`@�z�@�A�@�1@��m@���@�ƨ@�ƨ@��F@��F@���@�K�@��H@�~�@�=q@��@��@���@�`B@�/@�%@��@���@��@���@��@�j@�bN@�Q�@�A�@�b@��;@���@�C�@�"�@���@��y@��@��\@��@���@���@��#@��-@�`B@�V@�Ĝ@�r�@�Z@�A�@�A�@�1'@��@\)@~�@~��@~v�@~{@}@}�h@}��@}?}@|�@|�@|j@|9X@|(�@|1@{ƨ@{dZ@z��@z��@z�\@z^5@z=q@z�@y&�@x�@xQ�@x1'@xb@w�w@w�P@w+@v�@u�@u�h@u?}@t��@t�j@tZ@s��@s�
@sƨ@s�
@s�
@s�
@s�F@sC�@s@rn�@q�@q��@q��@q�7@q�7@q�7@qx�@q&�@p�`@p�@pA�@p  @o�w@o|�@ol�@o\)@n�R@n$�@n@m�T@m`B@l�@l�@lZ@l�@k�
@k�F@k�@j�@j��@jn�@i�@iG�@h��@h��@hĜ@h�u@hbN@g�@g;d@f�R@fv�@fE�@e�-@ep�@eO�@d�/@d9X@c�m@c�F@b��@bM�@ahs@a&�@`�@` �@_��@_�P@_;d@^�R@^V@^V@]p�@\�@\j@\�@[��@[dZ@[o@Z=q@YX@Y&�@Y�@X�`@X1'@W|�@Vȴ@U�@UO�@U�@T��@S��@S�
@Sƨ@S�F@S�@SC�@So@S@R�@R��@R��@RM�@Q�^@Q�7@Qx�@Qhs@Q7L@P�u@PA�@O��@Ol�@O+@N��@N�@N�+@N{@M@M�@M`B@MV@L�@L�j@Lj@L(�@K�
@Kt�@K33@J�@J��@J��@J~�@JM�@J-@I��@I7L@HĜ@H�@H �@G�w@G�P@G|�@G|�@Gl�@G;d@F�R@FE�@F{@E��@E�@D�j@D�D@C�
@C33@B�!@BJ@A��@AX@A%@@��@@r�@@ �@?�@?K�@?+@>�y@>$�@=�@<�@<��@<I�@<�@;��@;ƨ@;t�@:�@:�!@:=q@9��@9�#@9��@9x�@97L@8��@8�9@8�@8bN@8Q�@8 �@7�;@7�P@7l�@7�@6�y@6�@6��@6v�@6E�@5�-@5p�@5V@4�@4(�@4�@41@41@3��@3�F@3��@3�@3dZ@3S�@3C�@333@3"�@2��@2n�@2=q@2-@2�@2J@1��@1�@1�#@1��@1X@1G�@1&�@1�@0�`@0�u@0Q�@01'@0  @/��@/|�@/\)@/;d@.��@.��@.�+@.E�@.$�@-�@-�h@-p�@-?}@-�@,��@,�@+�m@+ƨ@+��@+dZ@+33@+"�@*�H@*�!@*��@*��@*�\@*~�@*M�@*J@)��@)�@)�@)��@)hs@)X@)X@)7L@)%@(�`@(�`@(�9@(Q�@'�@'�@'�P@'\)@'+@'�@'�@'�@&�y@&ȴ@&�+@&V@&E�@&{@%��@%p�@%/@%V@$�/@$��@$��@$j@#�m@#�F@#��@#��@#C�@#o@#o@"�!@"n�@"M�@"J@!��@!��@!7L@!%@ Ĝ@ �@ bN@ bN@ Q�@ Q�@ A�@ 1'@ b@�@��@��@�P@|�@l�@K�@+@
=@�y@�R@v�@ff@E�@5?@$�@�T@�-@�@`B@/@�/@�j@�D@9X@33@�H@��@n�@J@��@7L@�@��@��@r�@bN@bN@Q�@1'@�;@�@\)@;d@�R@ff@$�@@@��@`B@`B@O�@/@V@�/@��@z�@Z@I�@I�@9X@(�@�@��@�@S�@S�@C�@�\@^5@M�@M�@-@�@hs@��@�9@�9@�9@��@�@1'@b@�@�@l�@K�@�@
=@�y@�@ȴ@�+@5?@{@{@{@@�@��@@�-@�h@`B@V@��@��@��@j@I�@I�@(�@�@�
@��@�@dZ@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��/A��yA��yA��yA��yA��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A��A��#A��!A���A��uA��PA�~�A�jA�\)A�I�A�/A���A���A�p�A�K�A�+A�ȴA��7A�-A��\A�7LA���A��A���A��7A��A��A�ffA�p�A��FA�XA��`A��/A���A�9XA�v�A��-A��jA���A�E�A���A�ffA���A��-A��
A�(�A�{A�$�A�/A���A�\)A��jA�^5A�oA��mA���A��RA�p�A�S�A��7A��FA��DA�jA�G�A�
=A�S�A���A�+A�n�A��FA�O�A���A�&�A��RA�I�A�Q�A�A��yA��mA�
=A{��AyK�Aw�
Av�As��Ar=qAoAm\)Ak/Ag%Ad=qAa�A_�hA]VA[&�AY�;AY��AX~�AU&�ARZAQx�AN��AM�-AL��ALAJ��AG�AE�FAC�AB�ABA�A@��A@��A?��A?G�A>�`A>A�A=��A<��A<(�A9��A8bNA6ĜA5l�A4�!A4bNA3��A3;dA2�A0bNA-�mA,z�A,{A*1A(�RA'�PA&E�A%|�A$E�A"bNA�A|�AVAp�AC�A1'A`BA��A��Ar�A�mAƨA�Az�AVA�uAffA{AƨA��A  A��A
�yA	S�A�jAjAoA�A�A��A$�A ��@�V@��@��
@�dZ@�n�@�J@���@��`@�Z@���@�hs@�\@��/@�z�@�I�@��@��y@�@��`@�(�@�\)@�~�@���@���@�ff@���@�?}@�ƨ@��/@��
@��H@�n�@ݲ-@��@��`@�9X@�M�@ش9@�  @�33@ԛ�@�o@�=q@�-@�?}@�K�@���@��/@�l�@�J@ə�@ɉ7@ǅ@ź^@�O�@��@Ĵ9@�\)@�hs@�b@�;d@�ȴ@��h@��@��;@�l�@�5?@�7L@�j@�A�@�+@��T@��@�Q�@�  @�ƨ@�l�@�;d@���@�=q@�5?@��@���@�`B@�G�@�7L@�Ĝ@��j@�z�@��P@�K�@�K�@��H@�5?@�V@�I�@�A�@�9X@�9X@��@��@���@�V@�J@���@��/@�A�@�1@�dZ@���@�E�@��@��h@��@�z�@�bN@�A�@�1@��
@�t�@���@���@���@�/@���@�Ĝ@��9@���@��D@�A�@��P@�C�@�"�@��y@���@��R@��+@���@�p�@�G�@�7L@�7L@�/@�/@�Ĝ@��;@���@�|�@�;d@�@��@��@��!@�~�@�E�@��-@�&�@���@�Ĝ@��@�b@�ƨ@�\)@���@���@�n�@�E�@�@��T@���@�?}@�&�@��/@���@��D@�z�@�Z@�(�@�b@��m@��@��P@�t�@�S�@�33@��@�~�@���@��^@��@�X@�/@�&�@���@��@��/@��D@�A�@�1'@�(�@� �@��@�b@�1@���@�t�@�C�@��@��@���@�V@�E�@�=q@��@��#@��7@�p�@�`B@�O�@�G�@��@�%@��j@��@���@�r�@�Q�@�Q�@�9X@� �@��@� �@� �@�1@��m@��w@�dZ@�;d@���@���@���@�n�@�J@�@���@���@��7@�G�@�/@��@��@��`@�z�@�A�@�1@��m@���@�ƨ@�ƨ@��F@��F@���@�K�@��H@�~�@�=q@��@��@���@�`B@�/@�%@��@���@��@���@��@�j@�bN@�Q�@�A�@�b@��;@���@�C�@�"�@���@��y@��@��\@��@���@���@��#@��-@�`B@�V@�Ĝ@�r�@�Z@�A�@�A�@�1'@��@\)@~�@~��@~v�@~{@}@}�h@}��@}?}@|�@|�@|j@|9X@|(�@|1@{ƨ@{dZ@z��@z��@z�\@z^5@z=q@z�@y&�@x�@xQ�@x1'@xb@w�w@w�P@w+@v�@u�@u�h@u?}@t��@t�j@tZ@s��@s�
@sƨ@s�
@s�
@s�
@s�F@sC�@s@rn�@q�@q��@q��@q�7@q�7@q�7@qx�@q&�@p�`@p�@pA�@p  @o�w@o|�@ol�@o\)@n�R@n$�@n@m�T@m`B@l�@l�@lZ@l�@k�
@k�F@k�@j�@j��@jn�@i�@iG�@h��@h��@hĜ@h�u@hbN@g�@g;d@f�R@fv�@fE�@e�-@ep�@eO�@d�/@d9X@c�m@c�F@b��@bM�@ahs@a&�@`�@` �@_��@_�P@_;d@^�R@^V@^V@]p�@\�@\j@\�@[��@[dZ@[o@Z=q@YX@Y&�@Y�@X�`@X1'@W|�@Vȴ@U�@UO�@U�@T��@S��@S�
@Sƨ@S�F@S�@SC�@So@S@R�@R��@R��@RM�@Q�^@Q�7@Qx�@Qhs@Q7L@P�u@PA�@O��@Ol�@O+@N��@N�@N�+@N{@M@M�@M`B@MV@L�@L�j@Lj@L(�@K�
@Kt�@K33@J�@J��@J��@J~�@JM�@J-@I��@I7L@HĜ@H�@H �@G�w@G�P@G|�@G|�@Gl�@G;d@F�R@FE�@F{@E��@E�@D�j@D�D@C�
@C33@B�!@BJ@A��@AX@A%@@��@@r�@@ �@?�@?K�@?+@>�y@>$�@=�@<�@<��@<I�@<�@;��@;ƨ@;t�@:�@:�!@:=q@9��@9�#@9��@9x�@97L@8��@8�9@8�@8bN@8Q�@8 �@7�;@7�P@7l�@7�@6�y@6�@6��@6v�@6E�@5�-@5p�@5V@4�@4(�@4�@41@41@3��@3�F@3��@3�@3dZ@3S�@3C�@333@3"�@2��@2n�@2=q@2-@2�@2J@1��@1�@1�#@1��@1X@1G�@1&�@1�@0�`@0�u@0Q�@01'@0  @/��@/|�@/\)@/;d@.��@.��@.�+@.E�@.$�@-�@-�h@-p�@-?}@-�@,��@,�@+�m@+ƨ@+��@+dZ@+33@+"�@*�H@*�!@*��@*��@*�\@*~�@*M�@*J@)��@)�@)�@)��@)hs@)X@)X@)7L@)%@(�`@(�`@(�9@(Q�@'�@'�@'�P@'\)@'+@'�@'�@'�@&�y@&ȴ@&�+@&V@&E�@&{@%��@%p�@%/@%V@$�/@$��@$��@$j@#�m@#�F@#��@#��@#C�@#o@#o@"�!@"n�@"M�@"J@!��@!��@!7L@!%@ Ĝ@ �@ bN@ bN@ Q�@ Q�@ A�@ 1'@ b@�@��@��@�P@|�@l�@K�@+@
=@�y@�R@v�@ff@E�@5?@$�@�T@�-@�@`B@/@�/@�j@�D@9X@33@�H@��@n�@J@��@7L@�@��@��@r�@bN@bN@Q�@1'@�;@�@\)@;d@�R@ff@$�@@@��@`B@`B@O�@/@V@�/@��@z�@Z@I�@I�@9X@(�@�@��@�@S�@S�@C�@�\@^5@M�@M�@-@�@hs@��@�9@�9@�9@��@�@1'@b@�@�@l�@K�@�@
=@�y@�@ȴ@�+@5?@{@{@{@@�@��@@�-@�h@`B@V@��@��@��@j@I�@I�@(�@�@�
@��@�@dZ@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB	7BJBPBVBbBuB�B�B �B+B49B9XB<jB?}BJ�BP�B]/Bk�Bo�Br�Bn�Bm�BjBgmBcTB]/BYBT�BS�BS�BW
BA�B:^B/B!�B��B�NB�/B�5B�#B��B�}B�LB�=Bw�BffB.B"�BuB
��B
��B
��B
��B
��B
��B
��B
��B  B
��B
�B
�B
�B
�B
�`B
�5B
�B
��B
ǮB
B
�dB
�'B
�B
��B
��B
�B
q�B
_;B
K�B
+B
�B
	7B	��B	�TB	�B	�}B	�3B	��B	�B	ffB	N�B	@�B	/B	�B	�B	�B	bB	B�B�yB�HB�B��B��BǮB�XB��B��B��B��B��B��B��B��B��B��B��B�hB�PB�=B�B� By�Bw�Bv�Bt�Bq�Bo�Bk�BffBbNBaHB^5B[#BXBW
BS�BQ�BM�BK�BG�BB�B@�B?}B<jB:^B9XB6FB0!B0!B.B-B,B(�B'�B&�B&�B%�B$�B%�B$�B$�B%�B#�B"�B!�B �B�B$�B#�B%�B"�B#�B$�B$�B%�B%�B%�B&�B&�B&�B(�B,B+B+B+B+B+B+B+B)�B+B)�B+B)�B)�B(�B(�B'�B)�B(�B)�B(�B)�B(�B(�B(�B/B0!B.B/B33B33B33B33B49B33B49B5?B5?B5?B49B49B8RB7LB8RB9XB:^B=qB<jB<jB;dB;dB>wBA�BA�BC�BF�BI�BL�BM�BQ�BXBZB`BBdZBgmBp�Bv�B|�B�B�B�1B�JB�PB�VB�VB�hB��B��B��B��B��B��B��B��B�B�B�B�3B�?B�?B�LB�XB�dB�wBƨB��B��B��B��B�B�#B�/B�BB�HB�NB�TB�ZB�`B�mB�B�B�B��B��B��B��B�B��B��B	B	%B	+B		7B		7B	
=B	JB	oB	�B	�B	�B	�B	�B	�B	!�B	'�B	+B	,B	/B	1'B	2-B	33B	5?B	7LB	:^B	@�B	F�B	H�B	K�B	N�B	T�B	W
B	ZB	^5B	aHB	e`B	hsB	k�B	n�B	s�B	u�B	v�B	z�B	|�B	}�B	~�B	�B	�B	�B	�7B	�DB	�JB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�?B	�?B	�?B	�FB	�XB	�dB	�jB	�jB	�jB	�jB	�wB	�wB	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�TB	�`B	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B
DB

=B
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
,B
,B
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
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
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
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
gmB
gmB
gmB
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
hsB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	B0B6B"BHB[BmB�B �B*�B4B9$B<PB?cBJ�BP�B]BkkBoiBr�BncBmwBjeBgRBc:B\�BX�BT�BS�BS�BV�BAoB:DB/ B!�B��B�B��B�B�	B��B�cB�2B�#Bw�BfLB-�B"�B[B
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
�B
�B
�B
�B
�FB
�B
��B
��B
ǔB
�uB
�0B
�B
��B
��B
�yB
��B
qvB
_!B
K�B
*�B
YB
	B	��B	� B	�B	�cB	��B	�~B	��B	fLB	N�B	@OB	.�B	�B	xB	YB	HB	�B�wB�_B�B��B��BϫB�zB�$B��B�yB��B��B��B��B��B��B�qB�sB�MB�4B�B�	B��B�By�Bw�Bv�Bt�BqvBo�BkQBfLBbBaB^BZ�BW�BV�BS�BQ�BM�BK�BG�BB[B@OB?HB<PB:DB9$B6B/�B/�B-�B,�B+�B(�B'�B&�B&�B%�B$�B%�B$�B$�B%�B#�B"�B!�B �B�B$�B#�B%�B"�B#�B$�B$�B%�B%�B%�B&�B&�B&�B(�B+�B*�B*�B*�B*�B*�B*�B*�B)�B*�B)�B*�B)�B)�B(�B(�B'�B)�B(�B)�B(�B)�B(�B(�B(�B.�B0B-�B.�B3B3B2�B2�B4B2�B4B5B5%B5%B4B4B8B7B8B9>B:*B=<B<6B<6B;0B;0B>BBAUBAUBCaBFtBI�BL�BM�BQ�BW�BY�B`'Bd@BgRBpoBv�B|�B��B��B��B�B�B�"B�"B�4B�_B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�$B�0B�BBƎB˒B̘BбB��B��B�	B��B�B�B�B� B�&B�,B�RB�KB�WB�B��B��B��B��B�|B��B��B	�B	�B	�B		B		B	
	B	0B	:B	sB	eB	kB	�B	xB	xB	!�B	'�B	*�B	+�B	.�B	0�B	2B	2�B	5B	72B	:*B	@OB	F�B	H�B	K�B	N�B	T�B	V�B	ZB	^B	aB	e,B	h>B	kQB	ncB	s�B	u�B	v�B	z�B	|�B	}�B	~�B	��B	��B	��B	�B	�B	�B	�"B	�(B	�.B	�4B	�YB	�kB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�$B	�0B	�6B	�6B	�PB	�6B	�BB	�BB	�OB	�UB	�UB	�[B	�aB	�aB	�aB	�gB	āB	�gB	�gB	�gB	�mB	�mB	�zB	ȀB	ɆB	ʌB	˒B	̘B	��B	ϫB	бB	ҽB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�,B	�2B	�8B	�8B	�_B	�DB	�KB	�QB	�QB	�WB	�WB	�]B	�]B	�]B	�cB	�cB	�cB	�iB	�oB	�vB	�|B	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
	B

	B

	B

	B

	B
B

	B
B
)B
B
B
B
B
B
B
B
B
B
B
B
B
"B
BB
(B
(B
BB
.B
.B
.B
TB
@B
@B
@B
[B
aB
FB
FB
FB
aB
FB
MB
MB
MB
MB
SB
SB
YB
YB
YB
YB
YB
_B
yB
eB
eB
eB
�B
kB
�B
kB
qB
qB
qB
qB
qB
xB
xB
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
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/ B
/�B
/�B
/�B
/�B
1B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
4B
4B
4B
5B
5B
5B
5B
5B
5%B
5B
6B
6B
6B
7B
7B
8B
8B
9>B
9$B
:DB
:*B
;JB
;JB
;0B
<6B
<6B
<PB
<PB
=VB
=<B
=VB
>BB
?HB
?HB
?HB
@OB
@OB
@4B
@iB
@OB
AoB
AUB
B[B
B[B
B[B
B[B
B[B
CaB
CaB
CaB
D�B
DgB
DgB
DgB
DgB
DgB
DMB
EmB
EmB
EmB
EmB
EmB
EmB
FtB
FtB
FtB
GzB
GzB
G_B
GzB
GzB
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
[	B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
Z�B
Z�B
[�B
[�B
[�B
\B
[�B
[�B
\B
\B
\�B
\�B
\�B
]B
\�B
\�B
\�B
^B
^B
^B
^B
^B
^B
^B
`'B
`B
`'B
`B
aB
aB
aB
aB
bB
bB
bB
b4B
bB
bB
bB
c:B
c:B
d&B
d&B
e,B
e,B
e,B
e,B
e,B
e,B
f2B
f2B
f2B
f2B
f2B
g8B
g8B
g8B
g8B
g8B
g8B
gRB
g8B
g8B
g8B
h>B
h>B
h>B
h>B
iDB
jKB
jeB
jKB
jKB
jeB
jKB
kQB
kkB
kkB
kQB
kQB
kQB
lWB
lWB
lWB
lWB
lWB
m]B
m]B
m]B
m]B
m]B
mwB
ncB
n}B
ncB
ncB
ncB
n}B
ncB
oiB
oiB
o�B
oiB
oiB
o�B
oiB
oiB
oiB
poB
p�B
p�B
poB
p�B
poB
p�B
qvB
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.38(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803070038002018030700380020180307003800201804060311102018040603111020180406031110JA  ARFMdecpA19c                                                                20180302153512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180302063513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180302063514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180302063514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180302063515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180302063515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180302063515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180302063515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180302063515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180302063515                      G�O�G�O�G�O�                JA  ARUP                                                                        20180302065734                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180302153721  CV  JULD            G�O�G�O�F-                JM  ARCAJMQC2.0                                                                 20180306153800  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180306153800  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181110  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                