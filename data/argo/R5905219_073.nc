CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-04-18T09:38:04Z creation;2020-04-18T09:38:06Z conversion to V3.1      
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20200418093804  20200418095420  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               IA   JA                                  2B  A   APEX                            7906                            051216                          846 @��>��?1   @���$�@3��E���dL     1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D�fDfD�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1� D2  D2y�D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<�fD=  D=� D>  D>� D?fD?� D@  D@� D@��DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF�fDGfDG� DH  DH� DH��DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DPfDP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ D�|�Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�Dм�D���D�<�Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A  A$  AD  Ad  A�  A�  A�  A�  A�33A�33A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�L�B�L�B�� B�� B�� B�� B�� B�� B��3BĀ B�L�B�L�BЀ B�L�B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ CY�C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D��D D�fDfD�fD D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� DfD� D D� D D� D D� D D� D D� D D� D D�fD D� D D� D	�D� D D� D D� D D� D D� D  D � D! D!� D" D"� D#fD#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)�fD* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0��D1	�D1� D2 D2��D3	�D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;��D< D<�fD= D=� D> D>� D?fD?� D@ D@� DA	�DA� DB DB� DC	�DC� DD DD� DE DE� DF DF�fDGfDG� DH DH� DI	�DI� DJ DJ� DK DK��DL DL� DM DM� DN DN� DO DO� DPfDP�fDQfDQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_�fD` D`� Da Da� Db Db� Dc Dc� Dd Dd� DefDe� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt� Du Du� Dv Dv� Dw Dw� Dx Dx� Dy Dy� Dz Dz� D{ D{� D| D|� D} D}� D~ D~� D D� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D��D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D��D�H D�� D�� D� D�H D�� D��3D�3D�H D�� D�� D� D�H D�� D�� D� D�H D���D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D��3D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�K3D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D��D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�K3D�� D�� D� D�H D�� D�� D�3D�H D�� D�� D� D�H D���D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�K3D�� D�� D�3D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�K3D�� D�� D� D�H D�� D�� D� D�H D D�� D� D�H DÈ D�� D� D�H DĈ D�� D� D�H Dń�D���D� D�H Dƈ D�� D� D�H Dǈ D�� D� D�H DȈ D�� D� D�H DɈ D�� D� D�H Dʈ D�� D� D�H Dˈ D�� D� D�H D̈ D��3D� D�H D͋3D�� D� D�H DΈ D�� D� D�H Dψ D�� D� D�H DЄ�D���D��D�D�Dш D�� D� D�H D҈ D�� D� D�H Dӈ D�� D� D�H DԈ D�� D��D�H DՈ D�� D� D�H Dֈ D�� D� D�H D׈ D�� D� D�H D؈ D�� D� D�H Dو D�� D� D�H Dڈ D�� D� D�H Dۈ D�� D� D�H D܈ D�� D� D�H D݈ D�� D� D�H Dވ D�� D� D�H D߈ D�� D� D�H D�� D�� D� D�H D� D�� D� D�K3D� D�� D� D�H D� D�� D�3D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�K3D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D�3D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D��D�� D� D�H D� D�� D� D�H D� D�� D� D�D�D��D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�D�D�� D�� D� D�H D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A�bNA��/A�p�A�G�A�9XA�l�A�C�A�{A���A��TA��#A�t�A��
A˥�A�~�A��;A�Q�A��TA���AȅA�l�A�M�A��A��A�VA���Aŧ�Aŗ�AŇ+A�9XA�&�A�ȴA�bNA�A���A�-A�XA�A�I�A�hsA�A�A��A�x�A�|�A�t�A���A�?}A���A��DA�A��
A�I�A�A�`BA���A��PA��A�VA��!A��hA�;dA�$�A��RA�E�A���A�1'A���A���A�K�A��A�/A�A��/A���A�Q�A��jA���A��+A��A�bNA��mA�ffA�JA�ffA�n�A��;A���A�/A���A��A���A�1A��9A��jA��mA���A���A�A��A��7A���A�A�l�A�;dA���A�dZA�|�A���A}�Az��AwK�Au?}Aq��Ao?}AhȴAcVA_�A^E�A]�wA]K�A\9XAY?}AYAX��AVA�ATI�AR�AN�uAMl�AL��ALffAK?}AJZAH�\AG�hAGoAF��AF �AEC�AD��AD1AC�FACx�AB�\AA��AAS�A@�A>�A=��A<�A<n�A;��A:�+A9+A6v�A5t�A57LA4��A2r�A/��A-�A*�HA(��A&�A%��A$�yA#�A ~�AA�A��A �AbNAoA�^A%AVAS�A9XA�^A��A�-A�A\)A�A~�AbA��A�A��AA�At�Ar�A
(�A	A�RA��An�A+A��A-A��A��AVA&�A(�A7LA ff@�E�@�bN@�?}@���@��@�hs@�@�S�@�9X@��#@�I�@�;d@�R@��#@�?}@�Z@�  @��m@��
@�K�@��#@��@�hs@�u@�Z@��@�@�7L@���@�X@�E�@ޏ\@���@��H@�G�@�l�@١�@�p�@��H@ՙ�@�%@ӕ�@�~�@с@��/@�Ĝ@Ь@ϕ�@Χ�@��@̛�@�t�@ʇ+@�{@ɉ7@��@��/@ȣ�@�j@�I�@�I�@�b@ǝ�@Ǖ�@�|�@ǶF@�S�@��
@�ƨ@�|�@�@�X@ř�@őh@���@ě�@ă@ă@ēu@ċD@��;@Å@Å@Õ�@�|�@�t�@ÍP@Õ�@Õ�@�\)@°!@¸R@�~�@�V@�$�@��@�X@���@�r�@��;@�
=@�^5@�$�@�{@��@��h@�?}@��@�Q�@��@��!@�5?@���@���@���@��h@�p�@���@�1'@�dZ@���@��^@�?}@�X@���@��/@��j@�j@�|�@��\@���@�X@��@���@�Ĝ@�Q�@�|�@��@��y@���@�=q@��@��#@��@�V@���@�1'@���@�o@�v�@��^@���@��h@�p�@�7L@��@��`@���@�9X@��F@�|�@�l�@��@�@�ȴ@��+@�^5@�M�@�$�@�@��@��-@��7@�p�@��@���@��j@��@�  @��F@�dZ@�
=@��H@���@�v�@�M�@�5?@�-@��@���@��7@�hs@�%@���@�Q�@�9X@�b@�1@�ƨ@�\)@��@��@���@��@���@�x�@�`B@��@��@�A�@�(�@�b@��P@��@���@���@���@�~�@�5?@���@�hs@�p�@�G�@��@��@��j@�bN@� �@�1@�  @��@��w@�|�@�;d@��@�v�@�-@��T@�@�p�@�?}@�&�@�%@���@��@�A�@���@���@�"�@���@��R@�n�@��@��@��@�@�p�@�/@���@�z�@�(�@��@��m@���@�K�@���@���@���@��@�@���@��@�&�@��@���@��@�r�@�A�@��
@��P@�dZ@�;d@�+@�ȴ@��!@�v�@�^5@�5?@���@���@��@�X@�7L@�V@�z�@���@�l�@�;d@��@�@���@�^5@�@��h@�p�@�hs@�7L@��j@��u@�r�@�Q�@�b@���@���@�@���@�v�@�-@���@���@�X@�?}@���@��/@���@��j@��@�1'@�  @\)@~�@~�+@~ff@~{@~@}��@}�-@}`B@|�@|�j@{��@{t�@{33@z�H@z^5@y��@y�7@y&�@xĜ@xr�@x1'@w��@w��@w\)@w�@vff@u@u`B@uO�@uV@t�j@t�@t9X@t1@s��@s�@so@r��@r~�@q��@p�`@p��@o�;@o�P@n�y@nE�@n@m��@m��@m/@lz�@l9X@l1@k��@kƨ@k��@kdZ@kS�@j��@jJ@iX@iG�@h�`@h�u@hb@g��@gK�@f�@f��@fff@fV@fE�@f@e��@eO�@e/@d��@dj@c�m@c��@c��@c��@c�@cC�@b�H@bn�@a�#@aG�@`�`@` �@_��@_\)@_
=@^��@^$�@]�@]�-@\z�@[ƨ@[��@[��@[S�@[33@Z��@Z�@Y��@Y��@YX@Y7L@Y&�@Y&�@Y%@X��@X��@X�`@X�9@Xr�@W�@W��@Wl�@W�@V�R@V$�@U?}@T�D@Sƨ@SS�@R�H@R��@R^5@R=q@Q�@Q��@Qx�@Q7L@PĜ@PbN@P �@O�;@O��@O|�@N�@NV@N{@M�@M�h@Mp�@L��@L��@L�D@L1@K��@Ko@J��@J��@JM�@J-@JJ@I��@I�^@Ix�@I&�@H�9@HA�@G�@GK�@F�R@FV@F{@F@E�-@E?}@E�@D�@D��@D�j@DI�@C�F@C��@C33@C@B�H@B��@B��@B~�@B�@A�#@A��@AG�@A�@@�`@@�9@@�@@b@?\)@?
=@>�@>v�@>V@>E�@>{@=�T@=��@=��@=�h@=`B@<��@<�D@;��@;��@;�@;S�@;@:��@:�\@:^5@:=q@:J@9��@9hs@9%@8�9@8r�@8Q�@7�@7�@7��@7�P@7|�@7l�@7+@6ȴ@6v�@6E�@5�@5O�@4��@4��@4�j@4�@4�D@4j@4Z@4(�@41@3�
@3�F@3��@3��@3�@2�@2^5@2M�@2=q@2�@2�@2J@1�#@1��@1X@0Ĝ@0�@01'@/�;@/�w@/�w@/\)@.�y@.ȴ@.v�@.V@.@-��@-`B@,��@,��@,j@,9X@,�@,1@+�m@+��@+33@*��@*��@*n�@*=q@*J@)�#@)��@)�@(��@(Ĝ@(r�@( �@(  @'�@'�@'��@'�P@'\)@'+@'
=@&ȴ@&��@&��@&�+@&ff@&5?@%�T@%�h@%�@$�@$�@$�/@$��@$j@$1@#��@#t�@#S�@#"�@#o@"��@"�\@"=q@"�@"J@!��@!�@!�^@!X@!&�@ ��@ ��@ r�@ Q�@ 1'@ b@�w@��@K�@;d@+@�@
=@��@�@�R@��@v�@V@5?@{@��@�h@p�@?}@�@V@�@��@��@j@1@ƨ@�@dZ@S�@"�@�H@��@��@�\@M�@�@��@�@��@x�@G�@7L@%@��@��@r�@bN@A�@ �@  @�@�w@��@�P@;d@ȴ@v�@ff@V@5?@{@�@@��@�h@p�@`B@?}@�@�@z�@I�@(�@�@�@(�@(�@�@�m@�F@�F@�F@��@t�@S�@33@o@@��@��@=q@�@�^@�7@7L@��@��@Ĝ@��@bN@Q�@ �@ �@�@�@�P@l�@K�@+@
=@�y@��@��@v�@V@5?@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A�bNA��/A�p�A�G�A�9XA�l�A�C�A�{A���A��TA��#A�t�A��
A˥�A�~�A��;A�Q�A��TA���AȅA�l�A�M�A��A��A�VA���Aŧ�Aŗ�AŇ+A�9XA�&�A�ȴA�bNA�A���A�-A�XA�A�I�A�hsA�A�A��A�x�A�|�A�t�A���A�?}A���A��DA�A��
A�I�A�A�`BA���A��PA��A�VA��!A��hA�;dA�$�A��RA�E�A���A�1'A���A���A�K�A��A�/A�A��/A���A�Q�A��jA���A��+A��A�bNA��mA�ffA�JA�ffA�n�A��;A���A�/A���A��A���A�1A��9A��jA��mA���A���A�A��A��7A���A�A�l�A�;dA���A�dZA�|�A���A}�Az��AwK�Au?}Aq��Ao?}AhȴAcVA_�A^E�A]�wA]K�A\9XAY?}AYAX��AVA�ATI�AR�AN�uAMl�AL��ALffAK?}AJZAH�\AG�hAGoAF��AF �AEC�AD��AD1AC�FACx�AB�\AA��AAS�A@�A>�A=��A<�A<n�A;��A:�+A9+A6v�A5t�A57LA4��A2r�A/��A-�A*�HA(��A&�A%��A$�yA#�A ~�AA�A��A �AbNAoA�^A%AVAS�A9XA�^A��A�-A�A\)A�A~�AbA��A�A��AA�At�Ar�A
(�A	A�RA��An�A+A��A-A��A��AVA&�A(�A7LA ff@�E�@�bN@�?}@���@��@�hs@�@�S�@�9X@��#@�I�@�;d@�R@��#@�?}@�Z@�  @��m@��
@�K�@��#@��@�hs@�u@�Z@��@�@�7L@���@�X@�E�@ޏ\@���@��H@�G�@�l�@١�@�p�@��H@ՙ�@�%@ӕ�@�~�@с@��/@�Ĝ@Ь@ϕ�@Χ�@��@̛�@�t�@ʇ+@�{@ɉ7@��@��/@ȣ�@�j@�I�@�I�@�b@ǝ�@Ǖ�@�|�@ǶF@�S�@��
@�ƨ@�|�@�@�X@ř�@őh@���@ě�@ă@ă@ēu@ċD@��;@Å@Å@Õ�@�|�@�t�@ÍP@Õ�@Õ�@�\)@°!@¸R@�~�@�V@�$�@��@�X@���@�r�@��;@�
=@�^5@�$�@�{@��@��h@�?}@��@�Q�@��@��!@�5?@���@���@���@��h@�p�@���@�1'@�dZ@���@��^@�?}@�X@���@��/@��j@�j@�|�@��\@���@�X@��@���@�Ĝ@�Q�@�|�@��@��y@���@�=q@��@��#@��@�V@���@�1'@���@�o@�v�@��^@���@��h@�p�@�7L@��@��`@���@�9X@��F@�|�@�l�@��@�@�ȴ@��+@�^5@�M�@�$�@�@��@��-@��7@�p�@��@���@��j@��@�  @��F@�dZ@�
=@��H@���@�v�@�M�@�5?@�-@��@���@��7@�hs@�%@���@�Q�@�9X@�b@�1@�ƨ@�\)@��@��@���@��@���@�x�@�`B@��@��@�A�@�(�@�b@��P@��@���@���@���@�~�@�5?@���@�hs@�p�@�G�@��@��@��j@�bN@� �@�1@�  @��@��w@�|�@�;d@��@�v�@�-@��T@�@�p�@�?}@�&�@�%@���@��@�A�@���@���@�"�@���@��R@�n�@��@��@��@�@�p�@�/@���@�z�@�(�@��@��m@���@�K�@���@���@���@��@�@���@��@�&�@��@���@��@�r�@�A�@��
@��P@�dZ@�;d@�+@�ȴ@��!@�v�@�^5@�5?@���@���@��@�X@�7L@�V@�z�@���@�l�@�;d@��@�@���@�^5@�@��h@�p�@�hs@�7L@��j@��u@�r�@�Q�@�b@���@���@�@���@�v�@�-@���@���@�X@�?}@���@��/@���@��j@��@�1'@�  @\)@~�@~�+@~ff@~{@~@}��@}�-@}`B@|�@|�j@{��@{t�@{33@z�H@z^5@y��@y�7@y&�@xĜ@xr�@x1'@w��@w��@w\)@w�@vff@u@u`B@uO�@uV@t�j@t�@t9X@t1@s��@s�@so@r��@r~�@q��@p�`@p��@o�;@o�P@n�y@nE�@n@m��@m��@m/@lz�@l9X@l1@k��@kƨ@k��@kdZ@kS�@j��@jJ@iX@iG�@h�`@h�u@hb@g��@gK�@f�@f��@fff@fV@fE�@f@e��@eO�@e/@d��@dj@c�m@c��@c��@c��@c�@cC�@b�H@bn�@a�#@aG�@`�`@` �@_��@_\)@_
=@^��@^$�@]�@]�-@\z�@[ƨ@[��@[��@[S�@[33@Z��@Z�@Y��@Y��@YX@Y7L@Y&�@Y&�@Y%@X��@X��@X�`@X�9@Xr�@W�@W��@Wl�@W�@V�R@V$�@U?}@T�D@Sƨ@SS�@R�H@R��@R^5@R=q@Q�@Q��@Qx�@Q7L@PĜ@PbN@P �@O�;@O��@O|�@N�@NV@N{@M�@M�h@Mp�@L��@L��@L�D@L1@K��@Ko@J��@J��@JM�@J-@JJ@I��@I�^@Ix�@I&�@H�9@HA�@G�@GK�@F�R@FV@F{@F@E�-@E?}@E�@D�@D��@D�j@DI�@C�F@C��@C33@C@B�H@B��@B��@B~�@B�@A�#@A��@AG�@A�@@�`@@�9@@�@@b@?\)@?
=@>�@>v�@>V@>E�@>{@=�T@=��@=��@=�h@=`B@<��@<�D@;��@;��@;�@;S�@;@:��@:�\@:^5@:=q@:J@9��@9hs@9%@8�9@8r�@8Q�@7�@7�@7��@7�P@7|�@7l�@7+@6ȴ@6v�@6E�@5�@5O�@4��@4��@4�j@4�@4�D@4j@4Z@4(�@41@3�
@3�F@3��@3��@3�@2�@2^5@2M�@2=q@2�@2�@2J@1�#@1��@1X@0Ĝ@0�@01'@/�;@/�w@/�w@/\)@.�y@.ȴ@.v�@.V@.@-��@-`B@,��@,��@,j@,9X@,�@,1@+�m@+��@+33@*��@*��@*n�@*=q@*J@)�#@)��@)�@(��@(Ĝ@(r�@( �@(  @'�@'�@'��@'�P@'\)@'+@'
=@&ȴ@&��@&��@&�+@&ff@&5?@%�T@%�h@%�@$�@$�@$�/@$��@$j@$1@#��@#t�@#S�@#"�@#o@"��@"�\@"=q@"�@"J@!��@!�@!�^@!X@!&�@ ��@ ��@ r�@ Q�@ 1'@ b@�w@��@K�@;d@+@�@
=@��@�@�R@��@v�@V@5?@{@��@�h@p�@?}@�@V@�@��@��@j@1@ƨ@�@dZ@S�@"�@�H@��@��@�\@M�@�@��@�@��@x�@G�@7L@%@��@��@r�@bN@A�@ �@  @�@�w@��@�P@;d@ȴ@v�@ff@V@5?@{@�@@��@�h@p�@`B@?}@�@�@z�@I�@(�@�@�@(�@(�@�@�m@�F@�F@�F@��@t�@S�@33@o@@��@��@=q@�@�^@�7@7L@��@��@Ĝ@��@bN@Q�@ �@ �@�@�@�P@l�@K�@+@
=@�y@��@��@v�@V@5?@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�BB	�B	��B
&�B
)�B
0!B
>wB
R�B
S�B
O�B
N�B
Q�B
VB
ZB
cTB
gmB
hsB
iyB
ffB
ffB
�%B
�+B
�1B
�%B
�\B
�FB
��B
��B
��B
��B
��B
�B
�B
��B%B�B%�B33BJ�Bl�Bp�B�VB��B�XB�B�B�B��B
=BuB �BoBoBoB�B.B1'B/B.B/B-B1'B:^B9XB:^B8RB8RB6FB2-B%�B!�B�B�BB�B�TB�B��B��B��B�LB��B��B�bB�PBz�Bu�BhsBbNB_;BZBT�BQ�BM�B=qB+B�BoBB
�B
�mB
�#B
��B
��B
�wB
�FB
�-B
�B
��B
��B
�DB
v�B
_;B
G�B
8RB
�B
PB	�fB	��B	��B	��B	��B	��B	�{B	�B	� B	|�B	t�B	gmB	`BB	O�B	E�B	A�B	?}B	9XB	5?B	-B	'�B	$�B	!�B	"�B	�B	�B	�B	�B	{B	uB	JB	
=B	+B��B��B��B�B�B�B�fB�/B�B��B��B��B�wB�?B�B��B��B��B��B�{B�PB�DB�+B�B�B~�B{�Bz�By�B}�Bx�Bs�Bq�Bz�Bu�Bt�Bu�Bv�Bu�Bu�Bv�Bv�Bx�Bv�Bx�Bv�Bw�Br�Bq�Bp�Bo�Bp�Bm�Bm�Bk�Bk�Bm�Bk�BjBl�Bm�Bm�Bm�Bo�Bo�Bn�Bq�Br�Br�Bt�Bv�B|�B~�B� B�B�B�B�B�B�%B�+B�7B�hB�{B��B��B�B�-B�-B�'B��B��B��B�B�B�3B�B�B��B�XBÖBǮB��B�
B�`B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B	DB	bB	�B	�B	�B	�B	)�B	+B	5?B	:^B	>wB	?}B	A�B	G�B	I�B	L�B	M�B	M�B	N�B	O�B	R�B	VB	VB	W
B	YB	ZB	]/B	`BB	cTB	e`B	ffB	iyB	p�B	s�B	s�B	s�B	s�B	t�B	u�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�%B	�7B	�DB	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�PB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�?B	�?B	�FB	�RB	�XB	�dB	�jB	�qB	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
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
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
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
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
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
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
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
VB
VB
VB
VB
VB
W
B
W
B
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
^5B
^5B
^5B
^5B
_;B
_;B
`BB
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
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
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
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
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�BB	�B	��B
&�B
)�B
0!B
>wB
R�B
S�B
O�B
N�B
Q�B
VB
ZB
cTB
gmB
hsB
iyB
ffB
ffB
�%B
�+B
�1B
�%B
�\B
�FB
��B
��B
��B
��B
��B
�B
�B
��B%B�B%�B33BJ�Bl�Bp�B�VB��B�XB�B�B�B��B
=BuB �BoBoBoB�B.B1'B/B.B/B-B1'B:^B9XB:^B8RB8RB6FB2-B%�B!�B�B�BB�B�TB�B��B��B��B�LB��B��B�bB�PBz�Bu�BhsBbNB_;BZBT�BQ�BM�B=qB+B�BoBB
�B
�mB
�#B
��B
��B
�wB
�FB
�-B
�B
��B
��B
�DB
v�B
_;B
G�B
8RB
�B
PB	�fB	��B	��B	��B	��B	��B	�{B	�B	� B	|�B	t�B	gmB	`BB	O�B	E�B	A�B	?}B	9XB	5?B	-B	'�B	$�B	!�B	"�B	�B	�B	�B	�B	{B	uB	JB	
=B	+B��B��B��B�B�B�B�fB�/B�B��B��B��B�wB�?B�B��B��B��B��B�{B�PB�DB�+B�B�B~�B{�Bz�By�B}�Bx�Bs�Bq�Bz�Bu�Bt�Bu�Bv�Bu�Bu�Bv�Bv�Bx�Bv�Bx�Bv�Bw�Br�Bq�Bp�Bo�Bp�Bm�Bm�Bk�Bk�Bm�Bk�BjBl�Bm�Bm�Bm�Bo�Bo�Bn�Bq�Br�Br�Bt�Bv�B|�B~�B� B�B�B�B�B�B�%B�+B�7B�hB�{B��B��B�B�-B�-B�'B��B��B��B�B�B�3B�B�B��B�XBÖBǮB��B�
B�`B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B	DB	bB	�B	�B	�B	�B	)�B	+B	5?B	:^B	>wB	?}B	A�B	G�B	I�B	L�B	M�B	M�B	N�B	O�B	R�B	VB	VB	W
B	YB	ZB	]/B	`BB	cTB	e`B	ffB	iyB	p�B	s�B	s�B	s�B	s�B	t�B	u�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�%B	�7B	�DB	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�PB	�VB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�?B	�?B	�FB	�RB	�XB	�dB	�jB	�qB	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
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
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
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
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
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
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
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
VB
VB
VB
VB
VB
W
B
W
B
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
^5B
^5B
^5B
^5B
_;B
_;B
`BB
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
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
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
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
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200418183744  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200418093804  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200418093805  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200418093805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200418093806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200418093806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200418093806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200418093806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200418093806  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200418093806                      G�O�G�O�G�O�                JA  ARUP                                                                        20200418095420                      G�O�G�O�G�O�                