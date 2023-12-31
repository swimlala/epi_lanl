CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-17T09:35:41Z creation;2018-02-17T09:35:43Z conversion to V3.1;2019-12-23T06:26:52Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180217093541  20200120021524  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA  I2_0675_040                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�MVx��1   @�M`� @6�ۋ�q�b��2�W�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C&fC@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D�fD D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\fD\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt�fDu Du� Dv Dv� Dw Dw� Dx Dx� Dy Dy� Dz Dz� D{ D{� D| D|� D} D}� D~ D~� D D� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D�3D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D D�� D� D�H DÈ D�� D� D�H DĈ D�� D� D�H Dň D�� D� D�H Dƈ D�� D� D�H Dǈ D�� D� D�H DȈ D�� D� D�H DɈ D�� D� D�H Dʈ D�� D� D�H Dˈ D�� D� D�H D̈ D�� D� D�H D͈ D�� D� D�H DΈ D�� D� D�H Dψ D�� D� D�H DЈ D�� D� D�H Dш D�� D� D�H D҈ D�� D� D�H Dӈ D�� D� D�H DԈ D�� D� D�H DՈ D�� D� D�H Dֈ D�� D� D�H D׈ D�� D� D�H D؈ D�� D� D�H Dو D�� D� D�H Dڈ D�� D� D�H Dۈ D�� D� D�H D܈ D�� D� D�H D݈ D�� D� D�H Dވ D�� D� D�H D߈ D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D�3D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�D�D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D��3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�%A�%A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�bA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�1A�A�  A��A���A�x�A���A���A�I�A�A��PA�{A��TA���A�\)A�&�A���A��A��!A�l�A�?}A�jA�v�A�\)A��
A�ƨA��+A�VA���A��A�r�A���A�`BA�$�A�  A�-A��FA���A��A�S�A��A��A�bNA�(�A��#A��A�9XA�ȴA�z�A��A��A�Q�A���A��A�A�A��7A��HA�=qA���A��TA�r�A��yA�ZA�\)A��A��A�1'A�  A�7LA�oA� �A��+A�&�A���A�`BA���A��A���A�=qA��mA�jA�E�A� �A}��A|�+A|  A{��Aw��At�At��ArȴAn1'AjffAg�Ae��Ae�Ad�/Ad�Ab��A^(�A]�A\��A[/AY|�AXbAV��AVbNAT��AR��AKAI+AH5?AFbAD�/AD-AC�ACK�A@�A>�\A=�7A=K�A<�A;�mA9�-A6{A4�`A3��A2VA0�+A0=qA/x�A-�
A+�#A(��A'�A&��A%�A$�9A"�`A!�^A!33A �`A �A (�A�TA��A�-A��A��A��A�A(�A�A��Ap�A�A\)A��A�AE�Ax�A�AdZA�uAl�A	��A	%AA�AhsAĜA��A|�A�jA�A��A\)A �@�{@��;@��-@�\)@�J@�/@�bN@�"�@�z�@�@���@�9@���@��#@�"�@�=q@�7L@�@���@��@�M�@��@�C�@��@�&�@ܴ9@��@�M�@�{@��T@ف@�X@�7L@��m@�|�@�n�@�Q�@�b@���@���@�p�@�G�@Ь@��@�;d@�=q@̣�@��@˾w@ʟ�@�@ɩ�@�x�@���@��@�+@�X@��`@���@�z�@ÍP@�l�@�;d@���@¸R@�5?@�x�@��j@���@�J@�I�@��@�{@�x�@�A�@���@�
=@�n�@�V@�=q@���@� �@�ƨ@�dZ@��@���@�b@���@�@��!@�V@��#@���@��@�X@��9@��
@�l�@�
=@�ȴ@���@�E�@��^@��@��@�A�@��w@��P@��P@�|�@�|�@�t�@�\)@�C�@�"�@��+@�{@�p�@�&�@�/@�7L@�G�@�%@�Ĝ@�r�@��@�ƨ@��H@��\@�@�@��@���@��@��u@���@�K�@�M�@�J@��@�hs@���@��@�Z@�(�@�  @�C�@�v�@�J@���@���@���@���@���@���@���@���@��7@�x�@��`@�j@���@��w@���@��P@�"�@��H@���@�n�@�5?@���@�`B@�G�@�%@��j@�Q�@��F@�33@��@��R@�v�@�ff@�$�@���@���@�O�@�&�@��/@�bN@��F@��P@�l�@��@��H@��+@�E�@�$�@���@���@��-@��h@�x�@�G�@���@��@���@��D@�z�@�Z@�1@��
@�ƨ@��F@���@��P@�S�@��y@��+@�V@�-@��@���@���@��7@�x�@�O�@���@��`@��@��`@��/@��9@���@���@��@�A�@�A�@���@��@�t�@�dZ@�C�@�"�@�o@��y@���@��R@��!@���@���@��\@�v�@�ff@�^5@�V@�E�@�@��@��#@��^@��@�/@���@��@��`@���@��9@��9@���@�bN@�(�@�1@�  @��@��m@��w@���@��@�"�@��y@���@�v�@�-@���@��#@��^@���@�p�@�`B@�/@�V@��`@���@�j@� �@��@;d@~��@~��@}�h@}V@|��@|j@|(�@{ƨ@{"�@z�!@z~�@y�^@y�@x��@x�u@xb@w�P@wK�@w�@v�@v�+@vE�@v$�@u�-@up�@u?}@t��@t�j@tI�@s��@sƨ@s�@s33@r�H@r�!@rM�@q�^@q&�@p�9@pQ�@o�w@nȴ@n5?@m��@m?}@l��@l(�@k�
@kt�@ko@j�!@j~�@j^5@j=q@jJ@i�^@i7L@h��@hr�@hQ�@h �@g��@gl�@f��@fE�@e�T@e�@e�-@ep�@e`B@e/@d�@dI�@cƨ@co@b�!@b�\@bn�@b=q@aG�@`��@`r�@`r�@`Q�@`b@_�;@_��@_�w@_�@_��@_\)@^��@^��@^�+@]�T@]p�@]`B@]�@\�@\�@\9X@[�m@[�F@[33@Z��@Z�!@Zn�@Z-@ZJ@Y�@Y��@YG�@Y%@X��@X��@X�`@XĜ@XbN@W�@W�w@Wl�@W;d@V��@V��@Vv�@VE�@U��@U�h@UO�@UV@T��@T�D@Sƨ@S��@S�@SdZ@SS�@S33@R��@Q�@Q�#@Q��@Q�7@QG�@Q%@PbN@P1'@P  @OK�@O+@O+@O+@O+@O+@N��@N�@N�+@M��@M`B@MV@L��@L��@L�j@L�j@L�D@L�@K�
@K��@K�@K"�@K@J�H@J�!@J~�@J�@I�7@I%@H�@Hr�@HA�@H1'@H �@G�@G�@G+@Fv�@E��@E@E@E�@EO�@E�@D��@D��@D��@Dj@C�m@C�F@C�@CC�@Co@C@B�H@B�\@B�@A�#@Ax�@AX@AG�@@�9@@bN@@ �@?�@?�w@?��@?�P@?|�@?\)@?;d@?�@?
=@>��@>�@>��@>�+@>E�@>@=�h@=`B@=`B@=O�@=/@<��@<�D@<9X@;�
@;S�@;S�@;33@:�@:��@:�\@:=q@9��@9�#@9�^@9��@9�7@9x�@9hs@9G�@9%@8�9@8r�@8Q�@8 �@8  @8  @7�;@7��@7+@6ȴ@6�+@5�-@5`B@5V@4��@4��@4I�@49X@4(�@4(�@3��@3�
@3�
@3ƨ@3dZ@3@2�!@1��@1��@1X@1G�@1�@0�@0Q�@01'@0b@0  @/��@/��@/|�@/�@.�@.�+@.@-�-@-`B@-�@,�@,�D@,(�@,�@+�
@+��@+dZ@+"�@+@*��@*�!@*n�@*-@)��@)�^@)G�@(�`@(��@(�@(bN@(1'@(b@(  @'�P@'+@'�@'�@&��@&�y@&�+@&@%��@%�@%p�@%`B@%/@%V@$��@$�/@$��@$�@$�@$��@$9X@#ƨ@#�F@#�@#C�@#@"��@"~�@!��@!�#@!�^@!�7@!7L@!%@ �`@ ��@ r�@��@�@��@�P@K�@+@�@�R@�+@$�@��@��@@�h@?}@V@�/@�j@z�@I�@(�@�@1@1@1@��@�m@�F@�@"�@@�H@�H@�H@��@�!@=q@J@��@��@��@��@��@��@hs@%@�9@bN@  @�w@�P@|�@l�@\)@K�@;d@+@
=@��@�@�R@�+@V@V@V@$�@{@�@�T@��@O�@/@�@�/@�D@Z@�@�m@�m@�m@��@�@t�@33@�@��@�\@n�@=q@�#@��@x�@X@�@�`@Ĝ@��@�u@r�@ �@��@�@��@�P@\)@��@��@��@�+@ff@E�@E�@{@�-@�h@`B@O�@O�@?}@�@��@�@z�@I�@9X@1@�m@�F@�@t�@33@o@
�@
��@
��@
��@
�\@
n�@
^5@
M�@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�%A�%A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�bA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�1A�A�  A��A���A�x�A���A���A�I�A�A��PA�{A��TA���A�\)A�&�A���A��A��!A�l�A�?}A�jA�v�A�\)A��
A�ƨA��+A�VA���A��A�r�A���A�`BA�$�A�  A�-A��FA���A��A�S�A��A��A�bNA�(�A��#A��A�9XA�ȴA�z�A��A��A�Q�A���A��A�A�A��7A��HA�=qA���A��TA�r�A��yA�ZA�\)A��A��A�1'A�  A�7LA�oA� �A��+A�&�A���A�`BA���A��A���A�=qA��mA�jA�E�A� �A}��A|�+A|  A{��Aw��At�At��ArȴAn1'AjffAg�Ae��Ae�Ad�/Ad�Ab��A^(�A]�A\��A[/AY|�AXbAV��AVbNAT��AR��AKAI+AH5?AFbAD�/AD-AC�ACK�A@�A>�\A=�7A=K�A<�A;�mA9�-A6{A4�`A3��A2VA0�+A0=qA/x�A-�
A+�#A(��A'�A&��A%�A$�9A"�`A!�^A!33A �`A �A (�A�TA��A�-A��A��A��A�A(�A�A��Ap�A�A\)A��A�AE�Ax�A�AdZA�uAl�A	��A	%AA�AhsAĜA��A|�A�jA�A��A\)A �@�{@��;@��-@�\)@�J@�/@�bN@�"�@�z�@�@���@�9@���@��#@�"�@�=q@�7L@�@���@��@�M�@��@�C�@��@�&�@ܴ9@��@�M�@�{@��T@ف@�X@�7L@��m@�|�@�n�@�Q�@�b@���@���@�p�@�G�@Ь@��@�;d@�=q@̣�@��@˾w@ʟ�@�@ɩ�@�x�@���@��@�+@�X@��`@���@�z�@ÍP@�l�@�;d@���@¸R@�5?@�x�@��j@���@�J@�I�@��@�{@�x�@�A�@���@�
=@�n�@�V@�=q@���@� �@�ƨ@�dZ@��@���@�b@���@�@��!@�V@��#@���@��@�X@��9@��
@�l�@�
=@�ȴ@���@�E�@��^@��@��@�A�@��w@��P@��P@�|�@�|�@�t�@�\)@�C�@�"�@��+@�{@�p�@�&�@�/@�7L@�G�@�%@�Ĝ@�r�@��@�ƨ@��H@��\@�@�@��@���@��@��u@���@�K�@�M�@�J@��@�hs@���@��@�Z@�(�@�  @�C�@�v�@�J@���@���@���@���@���@���@���@���@��7@�x�@��`@�j@���@��w@���@��P@�"�@��H@���@�n�@�5?@���@�`B@�G�@�%@��j@�Q�@��F@�33@��@��R@�v�@�ff@�$�@���@���@�O�@�&�@��/@�bN@��F@��P@�l�@��@��H@��+@�E�@�$�@���@���@��-@��h@�x�@�G�@���@��@���@��D@�z�@�Z@�1@��
@�ƨ@��F@���@��P@�S�@��y@��+@�V@�-@��@���@���@��7@�x�@�O�@���@��`@��@��`@��/@��9@���@���@��@�A�@�A�@���@��@�t�@�dZ@�C�@�"�@�o@��y@���@��R@��!@���@���@��\@�v�@�ff@�^5@�V@�E�@�@��@��#@��^@��@�/@���@��@��`@���@��9@��9@���@�bN@�(�@�1@�  @��@��m@��w@���@��@�"�@��y@���@�v�@�-@���@��#@��^@���@�p�@�`B@�/@�V@��`@���@�j@� �@��@;d@~��@~��@}�h@}V@|��@|j@|(�@{ƨ@{"�@z�!@z~�@y�^@y�@x��@x�u@xb@w�P@wK�@w�@v�@v�+@vE�@v$�@u�-@up�@u?}@t��@t�j@tI�@s��@sƨ@s�@s33@r�H@r�!@rM�@q�^@q&�@p�9@pQ�@o�w@nȴ@n5?@m��@m?}@l��@l(�@k�
@kt�@ko@j�!@j~�@j^5@j=q@jJ@i�^@i7L@h��@hr�@hQ�@h �@g��@gl�@f��@fE�@e�T@e�@e�-@ep�@e`B@e/@d�@dI�@cƨ@co@b�!@b�\@bn�@b=q@aG�@`��@`r�@`r�@`Q�@`b@_�;@_��@_�w@_�@_��@_\)@^��@^��@^�+@]�T@]p�@]`B@]�@\�@\�@\9X@[�m@[�F@[33@Z��@Z�!@Zn�@Z-@ZJ@Y�@Y��@YG�@Y%@X��@X��@X�`@XĜ@XbN@W�@W�w@Wl�@W;d@V��@V��@Vv�@VE�@U��@U�h@UO�@UV@T��@T�D@Sƨ@S��@S�@SdZ@SS�@S33@R��@Q�@Q�#@Q��@Q�7@QG�@Q%@PbN@P1'@P  @OK�@O+@O+@O+@O+@O+@N��@N�@N�+@M��@M`B@MV@L��@L��@L�j@L�j@L�D@L�@K�
@K��@K�@K"�@K@J�H@J�!@J~�@J�@I�7@I%@H�@Hr�@HA�@H1'@H �@G�@G�@G+@Fv�@E��@E@E@E�@EO�@E�@D��@D��@D��@Dj@C�m@C�F@C�@CC�@Co@C@B�H@B�\@B�@A�#@Ax�@AX@AG�@@�9@@bN@@ �@?�@?�w@?��@?�P@?|�@?\)@?;d@?�@?
=@>��@>�@>��@>�+@>E�@>@=�h@=`B@=`B@=O�@=/@<��@<�D@<9X@;�
@;S�@;S�@;33@:�@:��@:�\@:=q@9��@9�#@9�^@9��@9�7@9x�@9hs@9G�@9%@8�9@8r�@8Q�@8 �@8  @8  @7�;@7��@7+@6ȴ@6�+@5�-@5`B@5V@4��@4��@4I�@49X@4(�@4(�@3��@3�
@3�
@3ƨ@3dZ@3@2�!@1��@1��@1X@1G�@1�@0�@0Q�@01'@0b@0  @/��@/��@/|�@/�@.�@.�+@.@-�-@-`B@-�@,�@,�D@,(�@,�@+�
@+��@+dZ@+"�@+@*��@*�!@*n�@*-@)��@)�^@)G�@(�`@(��@(�@(bN@(1'@(b@(  @'�P@'+@'�@'�@&��@&�y@&�+@&@%��@%�@%p�@%`B@%/@%V@$��@$�/@$��@$�@$�@$��@$9X@#ƨ@#�F@#�@#C�@#@"��@"~�@!��@!�#@!�^@!�7@!7L@!%@ �`@ ��@ r�@��@�@��@�P@K�@+@�@�R@�+@$�@��@��@@�h@?}@V@�/@�j@z�@I�@(�@�@1@1@1@��@�m@�F@�@"�@@�H@�H@�H@��@�!@=q@J@��@��@��@��@��@��@hs@%@�9@bN@  @�w@�P@|�@l�@\)@K�@;d@+@
=@��@�@�R@�+@V@V@V@$�@{@�@�T@��@O�@/@�@�/@�D@Z@�@�m@�m@�m@��@�@t�@33@�@��@�\@n�@=q@�#@��@x�@X@�@�`@Ĝ@��@�u@r�@ �@��@�@��@�P@\)@��@��@��@�+@ff@E�@E�@{@�-@�h@`B@O�@O�@?}@�@��@�@z�@I�@9X@1@�m@�F@�@t�@33@o@
�@
��@
��@
��@
�\@
n�@
^5@
M�@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BÖBÖBÖBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBŢBŢBŢBŢBŢBŢBɺB��B��B��B��B��B��B��B��B��B�B�mBB�B-B33B?}BK�BQ�B[#BffBq�Bx�B{�Bz�B|�Bn�BE�BB�BD�BO�BQ�BO�BN�BF�B?}B;dB8RB49B2-B33B@�B1'B'�B%�B �B�B{BDB	7BBB��B��B�fB��BÖB�jB�-B��B�hB�1B~�Bt�Bp�BdZB^5BXBN�BC�B33B)�B#�BB
�B
�ZB
��B
��B
ÖB
�wB
�?B
�B
��B
m�B
dZB
^5B
T�B
F�B
:^B
+B
!�B
�B
�B
  B	�NB	�/B	��B	�LB	��B	�\B	~�B	z�B	w�B	t�B	hsB	R�B	G�B	E�B	?}B	6FB	/B	&�B	!�B	�B	%B��B�B��B�bB�By�Bu�Bp�B\)BM�BM�BJ�BH�BD�B>wB:^B5?B9XB?}B<jB9XB8RB33B.B �B �B �B �B!�B(�B)�B)�B,B.B.B-B+B+B2-B1'B1'B2-B0!B.B-B.B-B-B,B/B1'B2-B/B-B,B+B%�B#�B#�B#�B#�B$�B!�B!�B!�B �B�B �B#�B �B �B�B�B�B�B�B#�B"�B#�B$�B%�B(�B+B+B-B.B0!B0!B.B.B0!B49B7LB9XB=qB=qB<jB<jB;dB:^B:^B:^B9XB;dB=qBB�BE�BF�BH�BG�BI�BK�BK�BI�BH�BF�BF�BG�BH�BJ�BK�BL�BL�BM�BO�BO�BO�BP�BT�BYB[#B\)B_;B`BB`BB_;B^5BbNBdZBffBm�Bo�Bx�Bz�B}�B�B�B�B�1B�DB�PB�\B�hB��B��B��B��B�B�B�B�-B�3B�9B�LB�RB�XB�^B�jB�jB�wB�}BŢBɺB��B��B��B��B��B��B��B��B��B��B��B�
B�/B�BB�HB�HB�NB�mB�mB�sB�B�B�B��B��B��B	+B	
=B	\B	bB	�B	 �B	"�B	#�B	#�B	'�B	)�B	+B	.B	/B	0!B	8RB	?}B	C�B	G�B	H�B	H�B	H�B	I�B	J�B	N�B	P�B	Q�B	S�B	ZB	]/B	bNB	dZB	gmB	n�B	r�B	s�B	t�B	v�B	y�B	z�B	{�B	{�B	}�B	�B	�B	�7B	�PB	�VB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�FB	�RB	�XB	�XB	�XB	�^B	�dB	�qB	�wB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�#B	�)B	�)B	�/B	�5B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
hB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
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
(�B
(�B
(�B
(�B
(�B
)�B
)�B
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
-B
.B
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
0!B
0!B
1'B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
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
6FB
6FB
6FB
7LB
7LB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
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
F�B
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
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
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
ZB
ZB
ZB
ZB
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
hsB
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
n�B
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
p�B
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
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�{B�{B�{BāBāBĜBĜBāBāBāBāBāBāBňBňBňBňBňBňBɠBʦBˬBʦBˬBˬB��B��B��B��B�B�RB�ByB,�B3B?cBK�BQ�B[	BfLBq�Bx�B{�Bz�B|�Bn}BE�BBuBD�BO�BQ�BO�BN�BF�B?cB;JB88B4B2B3B@iB1B'�B%�B �BsBaB)B	BB�B��B��B�LB��B�{B�PB�B��B�NB�B~�Bt�Bp�Bd@B^BW�BN�BC{B3B)�B#�B�B
�B
�@B
��B
ʦB
�{B
�]B
�%B
��B
�gB
mwB
d@B
^B
T�B
F�B
:DB
*�B
!�B
�B
B	��B	�4B	�B	��B	�2B	��B	�BB	~�B	z�B	w�B	t�B	hXB	R�B	G�B	E�B	?cB	6+B	/ B	&�B	!�B	yB	BˬB��B��B�HB��By�Bu�Bp�B\BM�BM�BJ�BH�BD�B>]B:DB5%B9>B?cB<PB9>B88B3B-�B �B �B �B �B!�B(�B)�B)�B+�B-�B-�B,�B*�B*�B2B1B1B2B0B-�B,�B-�B,�B,�B+�B/ B1B2B/ B,�B+�B*�B%�B#�B#�B#�B#�B$�B!�B!�B!�B �B�B �B#�B �B �B�B~B�BqB�B#�B"�B#�B$�B%�B(�B*�B*�B,�B-�B0B/�B-�B-�B0B4B72B9>B=VB=VB<PB<PB;JB:*B:DB:*B9$B;JB=<BBuBE�BF�BH�BGzBI�BK�BK�BI�BH�BF�BF�BG�BH�BJ�BK�BL�BL�BM�BO�BO�BO�BP�BT�BX�B[	B\B_!B`'B`'B_!B^Bb4Bd@BfLBmwBo�Bx�Bz�B}�B��B��B��B�B�)B�6B�(B�NB�kB��B��B��B��B��B��B��B�B�B�2B�B�>B�DB�PB�PB�]B�cBňBɆB̳BοB��BϫBϫB��B��B��B��BѷB��B��B��B�B�-B�B�B�RB�8B�XB�eB�}B�B��B��B��B	B	
#B	(B	HB	gB	 �B	"�B	#�B	#�B	'�B	)�B	*�B	-�B	.�B	/�B	88B	?HB	C{B	G�B	H�B	H�B	H�B	I�B	J�B	N�B	P�B	Q�B	S�B	Y�B	]B	b4B	d@B	gRB	ncB	r�B	s�B	t�B	v�B	y�B	z�B	{�B	{�B	}�B	��B	�B	�B	�6B	�"B	�NB	�aB	�aB	�mB	�yB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�+B	�B	�$B	�$B	�>B	�DB	�JB	�VB	�]B	�BB	�cB	�cB	�iB	�uB	�{B	āB	ňB	ƎB	ƎB	ǔB	ɠB	̳B	͟B	͹B	ϫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	��B	�B	�B	�B	�B	�!B	�B	�-B	�4B	�:B	�@B	�&B	�@B	�,B	�FB	�FB	�2B	�2B	�LB	�8B	�RB	�RB	�>B	�XB	�XB	�_B	�DB	�eB	�kB	�WB	�wB	�]B	�wB	�wB	�wB	�cB	�}B	�B	�iB	�B	�B	��B	��B	�vB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
B
B
B
�B
B
�B
	B
	B
	B

#B

	B

#B

#B
)B
)B
)B
B
0B
0B
6B
B
B
B
"B
<B
<B
<B
BB
BB
HB
.B
NB
:B
[B
[B
@B
FB
aB
gB
gB
gB
mB
mB
mB
SB
SB
mB
sB
sB
YB
sB
sB
yB
yB
B
B
�B
kB
�B
qB
�B
�B
xB
�B
�B
�B
~B
~B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
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
(�B
(�B
(�B
(�B
(�B
)�B
)�B
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
,�B
-�B
-�B
-�B
-�B
-�B
-�B
/ B
/ B
/ B
0B
0B
/�B
0B
0B
0B
0�B
/�B
1B
1�B
2B
2B
2B
2�B
3B
2�B
3B
2�B
3B
4B
4B
4B
4B
4B
4B
4B
5%B
5%B
6+B
6+B
6+B
72B
7B
6+B
72B
7B
7B
88B
9$B
9>B
9$B
9>B
9>B
9>B
9$B
:DB
:DB
:DB
:DB
:DB
;0B
;JB
;JB
;JB
;JB
;0B
;0B
<PB
<PB
<6B
<PB
<6B
<PB
=<B
=<B
=<B
=VB
=VB
=<B
=VB
=VB
=VB
=<B
=VB
>]B
>]B
>]B
>]B
>]B
?HB
?HB
?cB
?HB
?cB
@iB
@OB
@iB
AoB
AoB
AUB
AoB
AUB
BuB
BuB
BuB
B[B
BuB
C{B
C{B
CaB
C{B
CaB
CaB
C{B
DgB
D�B
DgB
DgB
D�B
DgB
D�B
E�B
E�B
EmB
E�B
F�B
G�B
GzB
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
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
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
X�B
X�B
X�B
ZB
ZB
Y�B
ZB
[	B
[	B
Z�B
[	B
[	B
[	B
[�B
\B
\B
\B
[�B
\B
\B
]B
\�B
]B
]B
]B
^B
^B
^B
^B
^B
^B
_!B
_!B
_B
_!B
_!B
_!B
_!B
_!B
_B
`B
`'B
`'B
`'B
`'B
`'B
`B
aB
aB
a-B
a-B
a-B
a-B
a-B
a-B
a-B
a-B
b4B
b4B
c:B
c B
c B
c:B
c:B
c:B
c:B
c B
c:B
d@B
d@B
d@B
d&B
d&B
d@B
d&B
d&B
eFB
eFB
eFB
eFB
eFB
eFB
e,B
fLB
fLB
fLB
fLB
gRB
gRB
gRB
gRB
g8B
gRB
g8B
hXB
hXB
hXB
hXB
hXB
i_B
iDB
i_B
iDB
i_B
jKB
jKB
jeB
jeB
jeB
jeB
kQB
kkB
kQB
kkB
kkB
kkB
lqB
lqB
lqB
lqB
lqB
mwB
mwB
mwB
m]B
mwB
n}B
ncB
n}B
n}B
n}B
n}B
n}B
n}B
o�B
o�B
o�B
o�B
oiB
oiB
p�B
p�B
p�B
poB
p�B
p�B
p�B
qvB
q�B
qvB
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.25(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802230039472018022300394720180223003947201804060310482018040603104820180406031048JA  ARFMdecpA19c                                                                20180217183527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180217093541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180217093541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180217093542  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180217093542  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180217093542  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180217093543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180217093543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180217093543  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180217093543                      G�O�G�O�G�O�                JA  ARUP                                                                        20180217095554                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180217154141  CV  JULD            G�O�G�O�F�h�                JM  ARCAJMQC2.0                                                                 20180222153947  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180222153947  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181048  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021524                      G�O�G�O�G�O�                