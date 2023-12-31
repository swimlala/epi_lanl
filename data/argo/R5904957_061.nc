CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140816  20181024140816  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               =A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׾�+l�H1   @׾���ߦ@2������c��/��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      =A   A   A   @9��@y��@�  A   A   A@  Aa��A���A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�33B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D ��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  Dy�D��Dy�D  D� D��D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  Dy�D��Dy�D   D � D!fD!� D"  D"� D#  D#y�D#��D$� D%  D%y�D%��D&� D'  D'� D(fD(�fD)fD)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5fD5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DJ��DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D\��D]y�D^  D^� D^��D_� D`fD`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm�fDn  Dny�Dn��Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Ds��Dty�Dt��Duy�Dv  Dv� Dw  Dw� Dw��Dy��D�FfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @I��@���@�  A  A$  AD  Ae��A���A�  A�  A���A�  A�  A�  A�  B  B	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  BqffByffB�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�3B�3B� B� B�� B��3B��3B�� C @ C&fC@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2Y�C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv&fCx@ Cz@ C|&fC~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�,�C�,�C�  C�  C�  C�  C�  C�  C�3C�3C�3C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  D  D � D	�D��D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D	�D��D D� D D� D D��D	�D��D D� D	�D� D D� D D� D D� D D� D D��D	�D� D D��D	�D��D  D � D!fD!� D" D"� D# D#��D$	�D$� D% D%��D&	�D&� D' D'� D(fD(�fD)fD)� D* D*� D+ D+�fD, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1�fD2 D2� D3 D3� D4 D4� D5fD5� D6fD6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<��D=	�D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DIfDI� DJ DJ� DK	�DK� DLfDL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX	�DX��DY	�DY� DZ DZ� D[ D[� D\ D\� D]	�D]��D^ D^� D_	�D_� D`fD`� Da Da�fDb Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� DmfDm�fDn Dn��Do	�Do� Dp Dp� Dq Dq�fDr Dr� Ds Ds� Dt	�Dt��Du	�Du��Dv Dv� Dw Dw� Dx	�Dy��D�NfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A݁A�|�A�t�A�n�A�r�A�~�A݅A݃A݉7Aݙ�Aݕ�Aݛ�Aݙ�A��;A��/A��HA��;A��A���A���A�ƨA���Aݺ^AݬAݕ�A�l�A�?}A��A�ƨA�v�A�M�A۩�A�^5A�?}A��TA�Q�A�33A�XA�A��yA���A� �A��A��;A���A�XA�I�A�jA��yAͩ�A���A�~�A��A�A�|�A�E�A�ffAƟ�A�ȴA�^5A��HA�C�A��PA���A��-A�1A�E�A��A��PA��A�\)A��^A���A�ȴA���A��9A��yA���A�1A�^5A��^A�r�A�VA�G�A�$�A��A��/A��HA�l�A���A�A�ĜA���A��A��-A�-A���A�t�A�jA�x�A�bNA�ȴA���A��A�oA��A�bNA�oA�ffA��9A�A�dZA�jA�M�A��-A��A�%A�33A�S�A�(�A}�wA}�Az��Aw�PAt�\ArQ�Ao
=Al�AkG�Aj�uAidZAh1'Ag
=Ac`BAb  Aa�PA_�FA^�uA\ĜAZI�AX�AV��AT�!AR��AQl�AN�AL�HAJr�AIhsAHI�AGdZAF��AF1'AE&�AC��AC&�AB��AB{A@Q�A?oA=+A9��A7�;A6��A6bA5\)A3+A1��A1x�A1C�A0jA.�`A-|�A+�A*ffA*A)�A(��A(I�A(1A&�A$��A#��A"bNA!t�A ffA��A�HA=qAoAA~�Al�A"�AM�AS�A��AA��A��A�A��A�
A~�AQ�A�
Al�A�A%A+AdZAbA�uA�A
�A
�DA
$�AjA
=AbAO�A`BAE�@�l�@�l�@��R@��^@��@�O�@�b@�(�@�=q@�`B@�j@��-@�S�@�\@���@�7@�7@�V@���@���@�z�@���@��y@�^@�O�@���@�r�@ߕ�@��#@݁@ܬ@ܴ9@���@�%@�$�@Ցh@�?}@�&�@ԣ�@�^5@�&�@�V@�l�@��`@��\@���@�bN@�S�@��@��y@��-@�`B@��@���@�;d@�  @��@�^5@�-@��H@�ȴ@��w@��D@�&�@�/@�%@���@���@�r�@�ƨ@�+@��R@�ff@�J@�Ĝ@���@�o@���@�M�@�E�@�$�@�@���@���@��/@��/@���@��j@��D@�I�@�1'@��@���@��@��m@�ƨ@���@�C�@��@�ȴ@�^5@���@���@���@�p�@�/@��@���@�9X@��w@�l�@��@�=q@��#@�x�@�%@� �@���@�@��R@�~�@�E�@�{@���@�7L@�%@�j@���@���@��\@�v�@�5?@�@���@��@��@�hs@�%@��/@�Z@��@��@���@��P@�l�@�K�@��y@�v�@�=q@��#@��h@�hs@��`@�(�@�1@��;@�|�@�
=@���@��@�?}@�j@��@�b@�(�@�1'@�b@��@���@��@�%@��D@�r�@�1'@�1@��@��@�l�@�S�@��@��m@�  @��F@�\)@���@���@�ff@�-@���@�p�@��@���@�hs@��@��`@���@���@���@�r�@��;@��y@�J@��#@���@��@�%@���@��`@���@���@�Ĝ@��@��@���@��/@�Ĝ@��u@�bN@�1@���@��R@�5?@��T@���@��@�/@��`@��9@��D@�z�@�Z@�A�@��@��;@��F@��P@�33@�@��R@���@���@�^5@�{@���@���@�`B@��@���@�z�@�1'@��;@��w@��@�t�@�K�@��@�V@�=q@���@���@��-@��7@�&�@���@��D@���@��9@�j@��
@��P@�K�@�V@��\@w�W@f3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�A݁A�|�A�t�A�n�A�r�A�~�A݅A݃A݉7Aݙ�Aݕ�Aݛ�Aݙ�A��;A��/A��HA��;A��A���A���A�ƨA���Aݺ^AݬAݕ�A�l�A�?}A��A�ƨA�v�A�M�A۩�A�^5A�?}A��TA�Q�A�33A�XA�A��yA���A� �A��A��;A���A�XA�I�A�jA��yAͩ�A���A�~�A��A�A�|�A�E�A�ffAƟ�A�ȴA�^5A��HA�C�A��PA���A��-A�1A�E�A��A��PA��A�\)A��^A���A�ȴA���A��9A��yA���A�1A�^5A��^A�r�A�VA�G�A�$�A��A��/A��HA�l�A���A�A�ĜA���A��A��-A�-A���A�t�A�jA�x�A�bNA�ȴA���A��A�oA��A�bNA�oA�ffA��9A�A�dZA�jA�M�A��-A��A�%A�33A�S�A�(�A}�wA}�Az��Aw�PAt�\ArQ�Ao
=Al�AkG�Aj�uAidZAh1'Ag
=Ac`BAb  Aa�PA_�FA^�uA\ĜAZI�AX�AV��AT�!AR��AQl�AN�AL�HAJr�AIhsAHI�AGdZAF��AF1'AE&�AC��AC&�AB��AB{A@Q�A?oA=+A9��A7�;A6��A6bA5\)A3+A1��A1x�A1C�A0jA.�`A-|�A+�A*ffA*A)�A(��A(I�A(1A&�A$��A#��A"bNA!t�A ffA��A�HA=qAoAA~�Al�A"�AM�AS�A��AA��A��A�A��A�
A~�AQ�A�
Al�A�A%A+AdZAbA�uA�A
�A
�DA
$�AjA
=AbAO�A`BAE�@�l�@�l�@��R@��^@��@�O�@�b@�(�@�=q@�`B@�j@��-@�S�@�\@���@�7@�7@�V@���@���@�z�@���@��y@�^@�O�@���@�r�@ߕ�@��#@݁@ܬ@ܴ9@���@�%@�$�@Ցh@�?}@�&�@ԣ�@�^5@�&�@�V@�l�@��`@��\@���@�bN@�S�@��@��y@��-@�`B@��@���@�;d@�  @��@�^5@�-@��H@�ȴ@��w@��D@�&�@�/@�%@���@���@�r�@�ƨ@�+@��R@�ff@�J@�Ĝ@���@�o@���@�M�@�E�@�$�@�@���@���@��/@��/@���@��j@��D@�I�@�1'@��@���@��@��m@�ƨ@���@�C�@��@�ȴ@�^5@���@���@���@�p�@�/@��@���@�9X@��w@�l�@��@�=q@��#@�x�@�%@� �@���@�@��R@�~�@�E�@�{@���@�7L@�%@�j@���@���@��\@�v�@�5?@�@���@��@��@�hs@�%@��/@�Z@��@��@���@��P@�l�@�K�@��y@�v�@�=q@��#@��h@�hs@��`@�(�@�1@��;@�|�@�
=@���@��@�?}@�j@��@�b@�(�@�1'@�b@��@���@��@�%@��D@�r�@�1'@�1@��@��@�l�@�S�@��@��m@�  @��F@�\)@���@���@�ff@�-@���@�p�@��@���@�hs@��@��`@���@���@���@�r�@��;@��y@�J@��#@���@��@�%@���@��`@���@���@�Ĝ@��@��@���@��/@�Ĝ@��u@�bN@�1@���@��R@�5?@��T@���@��@�/@��`@��9@��D@�z�@�Z@�A�@��@��;@��F@��P@�33@�@��R@���@���@�^5@�{@���@���@�`B@��@���@�z�@�1'@��;@��w@��@�t�@�K�@��@�V@�=q@���@���@��-@��7@�&�@���@��D@���@��9@�j@��
@��P@�K�@�V@��\@w�W@f3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�%B
�1B
�+B
�7B
�=B
��B
��B
��B
�B
�'B
�-B
�3B
�9B
�FB
�RB
�^B
�wB
ĜB
��B
�#B
�`B
�B
��B\BhBoBbBVBVBJBhB�B.B=qBD�BG�BH�B�B�LB�^B�XBɺB�B�mB�B  B+BB��B�BS�B]/Bt�B{�B}�B� B}�Bu�BR�B/B,B&�B �B�B�B�BhB+B��B�B�B�sB�`B�ZB�`B�ZB�NB��B�^Bz�BZBH�B;dB2-B%�B�BB�BXB?}BA�BB�BF�BF�BC�B49B'�B�BoBVB
��B
�fB
�B
��B
��B
�B
�\B
�B
�B
z�B
o�B
dZB
VB
K�B
G�B
7LB
!�B
VB	��B	�yB	�B	��B	ɺB	��B	�XB	�!B	��B	�oB	�VB	�B	x�B	m�B	^5B	S�B	G�B	<jB	49B	-B	 �B	�B	
=B	B��B��B��B��B�B�B�yB�fB�NB�#B��B��BB�qB�LB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�=B�B�B�B�B�B~�B�1B�B�B�B��B��B��B��B��B�B�!B�B�B�B�-B�RB�RB�XB�^B�dB��B�
B�/B�mB	B�B�;B�`B�yB�TB��BɺB��B��B��BǮB��B��BȴBǮB��B��B��BƨBÖB��B�wB�qB�wB�wB��BĜBȴBǮBȴBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�/B�`B�fB�mB�sB�yB�HB��BɺBB�^B��B��B��B��B��B��B��B�B�B�B�B�LBƨB��B�B��B��B	  B	%B	DB	JB	\B	�B	�B	�B	�B	{B	{B	{B	�B	�B	 �B	$�B	%�B	'�B	'�B	(�B	(�B	)�B	0!B	1'B	1'B	1'B	2-B	33B	5?B	6FB	7LB	8RB	8RB	9XB	9XB	;dB	=qB	A�B	A�B	D�B	H�B	J�B	K�B	M�B	P�B	R�B	VB	XB	[#B	\)B	^5B	cTB	e`B	ffB	gmB	n�B	q�B	x�B	{�B	}�B	~�B	� B	�B	�B	�%B	�1B	�JB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�RB	�XB	�jB	�qB	�wB	��B	B	ÖB	ÖB	ÖB	B	��B	�}B	�}B	��B	ÖB	ĜB	ĜB	ÖB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�5B	�/B	�5B	�BB	�TB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
1B
1B
DB
DB

=B
	7B

=B
PB
bB
"�B
/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�%B
�1B
�+B
�7B
�=B
��B
��B
��B
�B
�'B
�-B
�3B
�9B
�FB
�RB
�^B
�wB
ĜB
��B
�#B
�`B
�B
��B\BhBoBbBVBVBJBhB�B.B=qBD�BG�BH�B�B�LB�^B�XBɺB�B�mB�B  B+BB��B�BS�B]/Bt�B{�B}�B� B}�Bu�BR�B/B,B&�B �B�B�B�BhB+B��B�B�B�sB�`B�ZB�`B�ZB�NB��B�^Bz�BZBH�B;dB2-B%�B�BB�BXB?}BA�BB�BF�BF�BC�B49B'�B�BoBVB
��B
�fB
�B
��B
��B
�B
�\B
�B
�B
z�B
o�B
dZB
VB
K�B
G�B
7LB
!�B
VB	��B	�yB	�B	��B	ɺB	��B	�XB	�!B	��B	�oB	�VB	�B	x�B	m�B	^5B	S�B	G�B	<jB	49B	-B	 �B	�B	
=B	B��B��B��B��B�B�B�yB�fB�NB�#B��B��BB�qB�LB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�=B�B�B�B�B�B~�B�1B�B�B�B��B��B��B��B��B�B�!B�B�B�B�-B�RB�RB�XB�^B�dB��B�
B�/B�mB	B�B�;B�`B�yB�TB��BɺB��B��B��BǮB��B��BȴBǮB��B��B��BƨBÖB��B�wB�qB�wB�wB��BĜBȴBǮBȴBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�/B�`B�fB�mB�sB�yB�HB��BɺBB�^B��B��B��B��B��B��B��B�B�B�B�B�LBƨB��B�B��B��B	  B	%B	DB	JB	\B	�B	�B	�B	�B	{B	{B	{B	�B	�B	 �B	$�B	%�B	'�B	'�B	(�B	(�B	)�B	0!B	1'B	1'B	1'B	2-B	33B	5?B	6FB	7LB	8RB	8RB	9XB	9XB	;dB	=qB	A�B	A�B	D�B	H�B	J�B	K�B	M�B	P�B	R�B	VB	XB	[#B	\)B	^5B	cTB	e`B	ffB	gmB	n�B	q�B	x�B	{�B	}�B	~�B	� B	�B	�B	�%B	�1B	�JB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�RB	�XB	�jB	�qB	�wB	��B	B	ÖB	ÖB	ÖB	B	��B	�}B	�}B	��B	ÖB	ĜB	ĜB	ÖB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�5B	�/B	�5B	�BB	�TB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
1B
1B
DB
DB

=B
	7B

=B
PB
bB
"�B
/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140816                              AO  ARCAADJP                                                                    20181024140816    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140816  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140816  QCF$                G�O�G�O�G�O�0               