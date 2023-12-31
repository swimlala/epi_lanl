CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-10-11T15:47:56Z creation;2021-10-11T15:47:58Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20211011154756  20211011185249  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @ٙ�L�A<1   @ٙ�q�r@3-V��dC��$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC�fC  C�C  C  C  C  C �C"  C#�fC&  C(  C*  C,  C.  C0  C1�fC3�fC6  C8  C:  C<  C>  C@  CA�fCD  CF�CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D��D� D	  D	y�D
  D
� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� DfD� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Dr��Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�3D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @�  AffA$  AD  Ad  A�  A�  A�  A�  A�  A�  A�33A�  B  B	  B  BffB!  B)  B1  B9  BA  BI  BQ  BY  Ba  BiffBq  By  B�� B�� B�� B�� B�� B�� B��3B��fB��3B�� B�� B�� B��3B��3B�� B�� B�L�BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C&fC&fC@ CY�C@ C@ C@ C@ C Y�C"@ C$&fC&@ C(@ C*@ C,@ C.@ C0@ C2&fC4&fC6@ C8@ C:@ C<@ C>@ C@@ CB&fCD@ CFY�CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CVY�CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�3C�  C�  C�,�C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D	�D� D D� D D� D D� D D� D	�D� D	 D	��D
 D
� D D��D D� D D�fD D� D D� D D� D D� DfD� D D� D D� DfD� D D� D D� D D� D D� D D� D D� D D� D D��D D� D D� D  D ��D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D-fD-� D. D.� D/ D/� D0 D0� D1 D1�fD2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8	�D8��D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR��DS	�DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr��Ds	�Ds� Dt Dt� Du Du�fDv Dv� Dw Dw� Dx Dx� Dy Dy� Dz Dz� D{ D{� D| D|� D} D}� D~ D~� D D� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D���D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D���D� D�K3D�� D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D��D�H D��3D�� D� D�H D�� D�� D� D�K3D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�D�D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D���D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D���D� D�H D�� D�� D� D�H D��3D�� D� D�K3D��3D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D�3D�K3D�� D�� D� D�H D D�� D� D�H DÈ D�� D� D�H DĈ D�� D� D�H Dň D�� D� D�H Dƈ D�� D� D�H Dǈ D�� D� D�H DȈ D�� D� D�H DɈ D�� D� D�H Dʈ D�� D� D�H Dˈ D�� D� D�H D̈ D�� D� D�H D͈ D�� D� D�H DΈ D�� D� D�H Dψ D�� D� D�H DЈ D�� D�3D�K3Dш D�� D� D�H D҈ D�� D� D�H Dӈ D�� D� D�H DԈ D�� D� D�H DՈ D�� D� D�H Dֈ D�� D�3D�H D׈ D�� D� D�H D؈ D�� D� D�H Dو D�� D� D�H Dڈ D�� D� D�H Dۈ D�� D� D�H D܈ D�� D� D�H D݈ D�� D� D�H Dވ D�� D� D�H D߈ D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D��3D� D�D�D� D�� D�3D�K3D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�D�D� D�� D� D�H D� D�� D� D�D�D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D�� D�� D� D�H D��3D�� D� D�H D��3D��3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�l�A�n�A�t�A�t�A�t�A�t�A�v�A�x�A�v�A�x�A�x�A�x�A�z�AځAځAځAڃAځAځA�~�A�~�AځAڃAڅAڅAڇ+Aڇ+Aڇ+AڃAځAځA�|�A�|�A�~�A�~�A�~�AځAځA�~�Aթ�A���A��/A���A�$�A�bNA���A�jA�ȴA��A��A���A��;A���A�9XA���A�`BA�A���A���A�ȴA�jA���A�G�A���A��mA���A��+A�G�A�S�A���A�jA��+A���A��PA���A��A���A��uA���A��A�ZA��uA��jA���A��A��HA�9XA��9A��+A�S�A���A��A���A��HA� �A��7A�ZA�S�A�A���A�Q�A�^5A��A���A�5?A�n�A��A�\)A��A���A���A�  A�Q�A| �Au�;As��Arv�Aq��AqoApZAn�AmC�Ai��AgS�Afr�Ad�RAat�A[\)AXffAWXAU��AS�AP�AI��AHn�AH{AG�^AG?}AF��AFr�AEƨAD�!AC�mAAƨA>A�A<9XA;VA:ȴA:1'A9�A8�A7��A5C�A4�A3�
A2�uA1�FA/�-A.�`A-��A,�+A+"�A)S�A(Q�A(  A'S�A&�A%|�A$��A#�A"~�A!t�A v�A�FA�HAG�A�RA�A��AoAjA��A�!AK�A��AȴAAȴA�A�A��A
(�A�HAjA{A�wA�yAl�A�Al�A�`A�jA�uAz�Al�A J@�G�@�`B@��
@���@�b@�E�@�7@��@�`B@���@��@陚@柾@��#@�?}@�@�@��@�ƨ@�5?@ܣ�@�I�@���@�C�@١�@��/@�(�@���@�$�@�?}@��
@��y@ѡ�@Гu@�l�@�-@�@�p�@��@�9X@�@�@ɲ-@ə�@�hs@ɲ-@��T@���@��`@�1'@ȃ@��@�-@���@�z�@�bN@�9X@�I�@��
@�@���@�@��^@��@�1@��y@�^5@���@�?}@��@��D@�b@��P@�33@��@��y@��\@�E�@�@�G�@�Q�@��+@�O�@�9X@�1'@� �@��@�V@���@�  @�@���@��\@�^5@��@��h@�%@�G�@�@���@��!@�`B@��@�A�@��P@�@��@��R@�V@��@���@���@�O�@�/@��@�X@���@��@��#@�`B@���@���@�A�@��;@��@���@���@�ff@��@�J@���@�V@���@�I�@��F@�\)@�"�@�ȴ@���@�5?@��T@�@��-@��#@��@�$�@�-@���@�
=@�+@���@��;@��@�  @�1@��
@��m@�  @��P@�l�@��P@�ƨ@��F@���@��w@�ƨ@���@�K�@�+@�o@���@��@���@�{@��@�@��@��-@�hs@�O�@�&�@���@���@��D@��u@�(�@���@�+@��@���@��H@���@���@�{@��#@���@�?}@���@���@��j@��@��u@�Z@���@��
@��@�"�@�
=@��y@���@���@��+@�V@�5?@�J@���@�p�@��@��@�V@�%@�V@���@���@�Ĝ@��9@�r�@�A�@�b@���@�;d@��@���@�$�@��^@�V@��`@���@�1@��;@���@�S�@��@���@�^5@�{@��@�@��-@��7@�O�@�%@��u@� �@���@�\)@�33@��H@���@���@��+@�ff@�^5@�=q@���@�p�@�7L@�%@���@��@��D@�z�@�Z@�1'@�1@��m@�C�@��+@�V@�-@�{@��T@�@�@��-@���@��@��@��@���@�Ĝ@��j@���@�bN@�9X@�  @��
@��F@��P@�t�@�S�@���@�~�@�ff@�=q@�{@��T@���@��h@�&�@�Ĝ@�r�@�1'@�;@�P@�@~E�@}@}p�@}p�@}�@|�@|z�@|9X@|(�@|(�@{ƨ@{t�@{"�@z��@z=q@y�^@x��@w|�@vȴ@vv�@v5?@u�@u@u��@u?}@t�j@sƨ@sdZ@s33@r�@rn�@q�^@q&�@pĜ@p��@p�@pA�@pb@o�@o�@o\)@o
=@n�+@nff@n$�@m��@m`B@l��@lI�@k��@kdZ@k33@k@j��@j��@j~�@j�@ihs@h�`@h�u@h �@g�@gK�@g�@f��@f@e/@d��@d�D@dZ@d1@c�m@cƨ@cS�@c33@b�\@a�7@a%@`Ĝ@`��@`�@`bN@` �@_�w@_\)@^�y@^��@^5?@]��@]O�@]�@\�/@\�@\z�@\(�@[�m@[dZ@Z��@Z�\@Z~�@Z^5@Y�@YX@X��@XQ�@W�w@W\)@W;d@V�y@Vȴ@V5?@U��@T�/@T��@T�D@T(�@St�@R�@R��@R^5@RJ@Q��@Qx�@Q�@P�9@P��@PbN@P  @O��@N��@Nȴ@N��@NV@N5?@N{@M/@L��@L�@L�@L�@K��@K33@J��@I��@Ihs@I%@H�9@HQ�@H1'@Hb@G�@G��@G��@G+@F��@F$�@E��@E�@E�@D��@D�@D��@D�@D��@DI�@C�F@C�@C33@B�@B�!@BJ@A��@A�^@A&�@@Ĝ@@�@@b@?��@?��@?|�@?K�@?�@>ff@=��@=�h@=/@=/@<�/@<�@<I�@;ƨ@;��@;�@;dZ@;33@:��@:�!@:^5@:M�@:=q@:-@9�#@9&�@8Ĝ@8��@8bN@81'@8  @7��@7|�@7l�@7;d@6�@6v�@6E�@65?@6@5@5��@5�@5/@4�@4�@4��@4z�@3��@3��@333@2��@2��@2^5@1��@1�^@1%@0��@0�9@0�u@0A�@/�@/�;@/�@/�@.�R@.ff@-��@-�@,�@,��@,�D@,j@,I�@+��@+��@+t�@+dZ@+dZ@+C�@*�@*~�@*^5@*=q@*�@)�@)�#@)��@)��@)�^@)�7@)hs@)X@)X@)7L@)%@(�`@(��@(��@(�@(A�@(  @'��@'l�@'\)@';d@&�y@&ȴ@&�+@&@%��@%p�@%?}@%/@%�@%V@$��@$�@$��@$9X@#��@#C�@#@"�!@"M�@!��@!�@!�#@!��@!�^@!��@!hs@!&�@ ��@ ��@ �u@ bN@ 1'@�;@�w@�@��@\)@�@�y@�R@��@v�@V@E�@5?@�@@��@`B@��@�/@��@�@�D@z�@9X@�m@��@S�@"�@�H@�!@�@�^@��@hs@hs@&�@�`@�9@�u@bN@1'@ �@ �@b@  @�@�;@��@l�@;d@;d@;d@��@�R@��@��@v�@v�@V@E�@5?@@��@p�@V@��@��@j@I�@9X@(�@(�@1@�
@��@��@�@33@"�@@��@�!@~�@^5@M�@=q@-@�@�#@��@x�@hs@G�@��@�9@�9@�@1'@��@�P@\)@�@�@�R@�+@v�@ff@E�@$�@@�T@@`B@?}@/@/@?}@?}@/@�@V@��@�j@z�@Z@�@��@ƨ@�F@�F@��@t�@
�@
��@
��@
��@
�\@
-@	�7@	G�@	&�@��@��@��@r�@Q�@Q�@A�@�;@�@|�@K�@K�@K�@+@�y@��@5?@@�-@p�@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�l�A�n�A�t�A�t�A�t�A�t�A�v�A�x�A�v�A�x�A�x�A�x�A�z�AځAځAځAڃAځAځA�~�A�~�AځAڃAڅAڅAڇ+Aڇ+Aڇ+AڃAځAځA�|�A�|�A�~�A�~�A�~�AځAځA�~�Aթ�A���A��/A���A�$�A�bNA���A�jA�ȴA��A��A���A��;A���A�9XA���A�`BA�A���A���A�ȴA�jA���A�G�A���A��mA���A��+A�G�A�S�A���A�jA��+A���A��PA���A��A���A��uA���A��A�ZA��uA��jA���A��A��HA�9XA��9A��+A�S�A���A��A���A��HA� �A��7A�ZA�S�A�A���A�Q�A�^5A��A���A�5?A�n�A��A�\)A��A���A���A�  A�Q�A| �Au�;As��Arv�Aq��AqoApZAn�AmC�Ai��AgS�Afr�Ad�RAat�A[\)AXffAWXAU��AS�AP�AI��AHn�AH{AG�^AG?}AF��AFr�AEƨAD�!AC�mAAƨA>A�A<9XA;VA:ȴA:1'A9�A8�A7��A5C�A4�A3�
A2�uA1�FA/�-A.�`A-��A,�+A+"�A)S�A(Q�A(  A'S�A&�A%|�A$��A#�A"~�A!t�A v�A�FA�HAG�A�RA�A��AoAjA��A�!AK�A��AȴAAȴA�A�A��A
(�A�HAjA{A�wA�yAl�A�Al�A�`A�jA�uAz�Al�A J@�G�@�`B@��
@���@�b@�E�@�7@��@�`B@���@��@陚@柾@��#@�?}@�@�@��@�ƨ@�5?@ܣ�@�I�@���@�C�@١�@��/@�(�@���@�$�@�?}@��
@��y@ѡ�@Гu@�l�@�-@�@�p�@��@�9X@�@�@ɲ-@ə�@�hs@ɲ-@��T@���@��`@�1'@ȃ@��@�-@���@�z�@�bN@�9X@�I�@��
@�@���@�@��^@��@�1@��y@�^5@���@�?}@��@��D@�b@��P@�33@��@��y@��\@�E�@�@�G�@�Q�@��+@�O�@�9X@�1'@� �@��@�V@���@�  @�@���@��\@�^5@��@��h@�%@�G�@�@���@��!@�`B@��@�A�@��P@�@��@��R@�V@��@���@���@�O�@�/@��@�X@���@��@��#@�`B@���@���@�A�@��;@��@���@���@�ff@��@�J@���@�V@���@�I�@��F@�\)@�"�@�ȴ@���@�5?@��T@�@��-@��#@��@�$�@�-@���@�
=@�+@���@��;@��@�  @�1@��
@��m@�  @��P@�l�@��P@�ƨ@��F@���@��w@�ƨ@���@�K�@�+@�o@���@��@���@�{@��@�@��@��-@�hs@�O�@�&�@���@���@��D@��u@�(�@���@�+@��@���@��H@���@���@�{@��#@���@�?}@���@���@��j@��@��u@�Z@���@��
@��@�"�@�
=@��y@���@���@��+@�V@�5?@�J@���@�p�@��@��@�V@�%@�V@���@���@�Ĝ@��9@�r�@�A�@�b@���@�;d@��@���@�$�@��^@�V@��`@���@�1@��;@���@�S�@��@���@�^5@�{@��@�@��-@��7@�O�@�%@��u@� �@���@�\)@�33@��H@���@���@��+@�ff@�^5@�=q@���@�p�@�7L@�%@���@��@��D@�z�@�Z@�1'@�1@��m@�C�@��+@�V@�-@�{@��T@�@�@��-@���@��@��@��@���@�Ĝ@��j@���@�bN@�9X@�  @��
@��F@��P@�t�@�S�@���@�~�@�ff@�=q@�{@��T@���@��h@�&�@�Ĝ@�r�@�1'@�;@�P@�@~E�@}@}p�@}p�@}�@|�@|z�@|9X@|(�@|(�@{ƨ@{t�@{"�@z��@z=q@y�^@x��@w|�@vȴ@vv�@v5?@u�@u@u��@u?}@t�j@sƨ@sdZ@s33@r�@rn�@q�^@q&�@pĜ@p��@p�@pA�@pb@o�@o�@o\)@o
=@n�+@nff@n$�@m��@m`B@l��@lI�@k��@kdZ@k33@k@j��@j��@j~�@j�@ihs@h�`@h�u@h �@g�@gK�@g�@f��@f@e/@d��@d�D@dZ@d1@c�m@cƨ@cS�@c33@b�\@a�7@a%@`Ĝ@`��@`�@`bN@` �@_�w@_\)@^�y@^��@^5?@]��@]O�@]�@\�/@\�@\z�@\(�@[�m@[dZ@Z��@Z�\@Z~�@Z^5@Y�@YX@X��@XQ�@W�w@W\)@W;d@V�y@Vȴ@V5?@U��@T�/@T��@T�D@T(�@St�@R�@R��@R^5@RJ@Q��@Qx�@Q�@P�9@P��@PbN@P  @O��@N��@Nȴ@N��@NV@N5?@N{@M/@L��@L�@L�@L�@K��@K33@J��@I��@Ihs@I%@H�9@HQ�@H1'@Hb@G�@G��@G��@G+@F��@F$�@E��@E�@E�@D��@D�@D��@D�@D��@DI�@C�F@C�@C33@B�@B�!@BJ@A��@A�^@A&�@@Ĝ@@�@@b@?��@?��@?|�@?K�@?�@>ff@=��@=�h@=/@=/@<�/@<�@<I�@;ƨ@;��@;�@;dZ@;33@:��@:�!@:^5@:M�@:=q@:-@9�#@9&�@8Ĝ@8��@8bN@81'@8  @7��@7|�@7l�@7;d@6�@6v�@6E�@65?@6@5@5��@5�@5/@4�@4�@4��@4z�@3��@3��@333@2��@2��@2^5@1��@1�^@1%@0��@0�9@0�u@0A�@/�@/�;@/�@/�@.�R@.ff@-��@-�@,�@,��@,�D@,j@,I�@+��@+��@+t�@+dZ@+dZ@+C�@*�@*~�@*^5@*=q@*�@)�@)�#@)��@)��@)�^@)�7@)hs@)X@)X@)7L@)%@(�`@(��@(��@(�@(A�@(  @'��@'l�@'\)@';d@&�y@&ȴ@&�+@&@%��@%p�@%?}@%/@%�@%V@$��@$�@$��@$9X@#��@#C�@#@"�!@"M�@!��@!�@!�#@!��@!�^@!��@!hs@!&�@ ��@ ��@ �u@ bN@ 1'@�;@�w@�@��@\)@�@�y@�R@��@v�@V@E�@5?@�@@��@`B@��@�/@��@�@�D@z�@9X@�m@��@S�@"�@�H@�!@�@�^@��@hs@hs@&�@�`@�9@�u@bN@1'@ �@ �@b@  @�@�;@��@l�@;d@;d@;d@��@�R@��@��@v�@v�@V@E�@5?@@��@p�@V@��@��@j@I�@9X@(�@(�@1@�
@��@��@�@33@"�@@��@�!@~�@^5@M�@=q@-@�@�#@��@x�@hs@G�@��@�9@�9@�@1'@��@�P@\)@�@�@�R@�+@v�@ff@E�@$�@@�T@@`B@?}@/@/@?}@?}@/@�@V@��@�j@z�@Z@�@��@ƨ@�F@�F@��@t�@
�@
��@
��@
��@
�\@
-@	�7@	G�@	&�@��@��@��@r�@Q�@Q�@A�@�;@�@|�@K�@K�@K�@+@�y@��@5?@@�-@p�@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>wB>wB>wB>wB>wB>wB=qB=qB>wB>wB>wB>wB>wB=qB=qB>wB=qB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�BA�BA�BB�BD�B@�B��B��BB%B�B/BB�BVBiyBm�Bp�Bw�B�%B�\B�hB�uB{�BiyB\)BffBhsBr�By�Bn�BdZBZBT�BP�BC�B;dBB�BE�BM�B@�B0!B�B��B��B�B�NB�B��B�RB�FB��B��B��B��B�=B�Bw�Bp�BiyBe`B\)BN�BE�B8RB#�BDB
��B
�B
�TB
�B
��B
B
�XB
��B
��B
�uB
�\B
�B
z�B
p�B
VB
)�B
�B
bB
DB
B
B	��B	�B	�B	ŢB	�qB	�3B	��B	�B	l�B	bNB	]/B	P�B	B�B	1'B	'�B	'�B	'�B	(�B	'�B	&�B	$�B	 �B	�B	�B	1B��B��B��B��B��B�B�B�B�B�B�B�B�B�ZB�HB�B��B��BŢBĜBĜBÖBB��B��B�wB�jB�jB�^B�XB�XB�FB�9B�3B�'B�9B�?B�!B��B��B�PB�7B�+B�B|�Bz�B{�B{�B|�B{�Bz�Bw�Bt�Bt�Bs�Bt�Bu�Bv�Bu�Bv�Bt�Bu�Bm�BiyBgmBiyBhsBhsBiyBhsBiyBhsBiyBjBiyBiyBk�Bl�Bk�Bk�Bm�Bn�Bo�Bs�Bu�Bt�Bs�Bt�Bv�Bv�Bx�Bz�B{�B~�B� B�B�7B�DB�JB�\B��B��B�uB�uB�{B�{B��B��B�'B�RB�jBB��B��BB��BĜBǮB��B��B��B��B��B�B�B�
B�B�B�
B�5B�TB�`B�fB�mB�mB�sB�sB�yB�yB�yB�yB�yB�`B�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	1B	VB	hB	oB	�B	�B	�B	#�B	'�B	(�B	+B	-B	.B	1'B	49B	7LB	8RB	:^B	>wB	C�B	E�B	N�B	S�B	T�B	T�B	W
B	XB	YB	[#B	^5B	bNB	cTB	e`B	jB	o�B	t�B	v�B	z�B	~�B	�B	�B	�+B	�DB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	�B	�B	�'B	�?B	�FB	�XB	�qB	�wB	��B	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�ZB	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
1B

=B
DB
JB
PB
PB
PB
JB
JB
JB
PB
VB
\B
\B
bB
oB
uB
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
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
(�B
)�B
+B
+B
,B
-B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
2-B
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
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
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
L�B
K�B
L�B
L�B
M�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
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
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
^5B
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
iyB
iyB
iyB
jB
jB
jB
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
p�B
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
v�B
v�B
w�B
w�B
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
}�B
}�B
~�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
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
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
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
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>wB>wB>wB>wB>wB>wB=qB=qB>wB>wB>wB>wB>wB=qB=qB>wB=qB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�BA�BA�BB�BD�B@�B��B��BB%B�B/BB�BVBiyBm�Bp�Bw�B�%B�\B�hB�uB{�BiyB\)BffBhsBr�By�Bn�BdZBZBT�BP�BC�B;dBB�BE�BM�B@�B0!B�B��B��B�B�NB�B��B�RB�FB��B��B��B��B�=B�Bw�Bp�BiyBe`B\)BN�BE�B8RB#�BDB
��B
�B
�TB
�B
��B
B
�XB
��B
��B
�uB
�\B
�B
z�B
p�B
VB
)�B
�B
bB
DB
B
B	��B	�B	�B	ŢB	�qB	�3B	��B	�B	l�B	bNB	]/B	P�B	B�B	1'B	'�B	'�B	'�B	(�B	'�B	&�B	$�B	 �B	�B	�B	1B��B��B��B��B��B�B�B�B�B�B�B�B�B�ZB�HB�B��B��BŢBĜBĜBÖBB��B��B�wB�jB�jB�^B�XB�XB�FB�9B�3B�'B�9B�?B�!B��B��B�PB�7B�+B�B|�Bz�B{�B{�B|�B{�Bz�Bw�Bt�Bt�Bs�Bt�Bu�Bv�Bu�Bv�Bt�Bu�Bm�BiyBgmBiyBhsBhsBiyBhsBiyBhsBiyBjBiyBiyBk�Bl�Bk�Bk�Bm�Bn�Bo�Bs�Bu�Bt�Bs�Bt�Bv�Bv�Bx�Bz�B{�B~�B� B�B�7B�DB�JB�\B��B��B�uB�uB�{B�{B��B��B�'B�RB�jBB��B��BB��BĜBǮB��B��B��B��B��B�B�B�
B�B�B�
B�5B�TB�`B�fB�mB�mB�sB�sB�yB�yB�yB�yB�yB�`B�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B	B	1B	VB	hB	oB	�B	�B	�B	#�B	'�B	(�B	+B	-B	.B	1'B	49B	7LB	8RB	:^B	>wB	C�B	E�B	N�B	S�B	T�B	T�B	W
B	XB	YB	[#B	^5B	bNB	cTB	e`B	jB	o�B	t�B	v�B	z�B	~�B	�B	�B	�+B	�DB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	�B	�B	�'B	�?B	�FB	�XB	�qB	�wB	��B	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�ZB	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
1B

=B
DB
JB
PB
PB
PB
JB
JB
JB
PB
VB
\B
\B
bB
oB
uB
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
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
(�B
)�B
+B
+B
,B
-B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
2-B
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
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
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
L�B
K�B
L�B
L�B
M�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
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
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
^5B
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
iyB
iyB
iyB
jB
jB
jB
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
p�B
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
v�B
v�B
w�B
w�B
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
}�B
}�B
~�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
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
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
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
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20211012004703  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20211011154756  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20211011154756  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20211011154756  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20211011154757  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20211011154757  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20211011154757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20211011154757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20211011154758  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20211011154758                      G�O�G�O�G�O�                JA  ARUP                                                                        20211011155228                      G�O�G�O�G�O�                JA  ARUP                                                                        20211011185249                      G�O�G�O�G�O�                