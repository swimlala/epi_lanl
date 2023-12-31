CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-12T21:41:10Z creation;2023-01-12T21:41:11Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230112214110  20230112222556  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @����1   @��?V�@;���R�d�O�;d1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  @���A   A@  A^ffA�  A�  A�  A�  A�  A���A�  A���A�33B��B  BffB   B)��B.ffB8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C�C  C  C�fC  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.�C0�C2  C3�fC6  C8�C9�fC<  C>  C?�fCB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C��3C��3C��C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D��D� D  D� DfD�fD  Dy�D  D� D��D� D	fD	� D
  D
� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��Dy�D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dw��Dxy�Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D���D�  D�@ D��3D�� D�  D�C3D�� D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�|�D���D�  D�@ D�� D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ D�|�Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D���D�@ Dʀ D�� D�3D�C3D˃3D�� D�  D�C3D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dփ3D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�  @���A   A@  A^ffA�  A�  A�  A�  A�  A���A�  A���A�33B��B  BffB   B)��B.ffB8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C�C  C  C�fC  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.�C0�C2  C3�fC6  C8�C9�fC<  C>  C?�fCB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C��3C��3C��C�  C�  C�  C��3C�  C�  C�  C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D��D� D  D� DfD�fD  Dy�D  D� D��D� D	fD	� D
  D
� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��Dy�D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dw��Dxy�Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D���D�  D�@ D��3D�� D�  D�C3D�� D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�|�D���D�  D�@ D�� D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ D�|�Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D���D�@ Dʀ D�� D�3D�C3D˃3D�� D�  D�C3D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dփ3D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��MA�zxA�n�A�ncA�f�A�d�A�o�A�tTA�q�A�lWA�Z�A�u�A���A�s�A�T,A�]�A�`�A�`�A�~A���A��A�H�A�n�A�$A���A��$A��:A��MA���A���A��SA���A��(A��A��~A���A���A��PA���A��JA��JA���A��{A��A���A��A�Q�A���A�IRA��A�\]A���A�`vA�+kA���A���A�LdA��A��PA��A�v`A��sA�MjA��;A�(XA��A��A�4�A��RA��.A��A��A�	A�5�A��A�.A� 'A��A��7A��wA���A�($A�#A���A�~�A�\]A��A��A��A���A��A�^5A�uA��A�$tA�b�A�u�A��A��AԕA�AP�A~�A}��A|�oA{|�AzkQAyc�Aw[WAv($At�#Ar�Ar�Aq��Ap�TAp	An�}AmH�Al\)AkA Ah�2AeR�AaԕA^u�A]˒A]��A]�A\��A\RTA\qA[��A[a�AZ�.AZC-AX��AW�rAVYAT�?AT��AT-�AS�`AR��AO֡AJ�/AI�RAI:�AH��AH,�AG&�AFb�AE��AEH�AD��AD�AD�:ADjAD6�AC��AC�XACc�AB�AB��AB�}AB�AB�+AB_pABQAA��AA�AA��AAs�AAS&AA$�A@��A@A A?҉A?�A=�SA;oiA9��A8	lA7_A71'A7�A6��A6a�A5i�A41'A2e,A0�A/�kA/�A-��A-oA+��A)XyA(��A(b�A(0UA'��A'͟A'�~A'�A&�FA&XA&4A%w2A%<6A%!-A%�A$��A$��A$0�A#�kA#�A"��A"�A">�A ��A��A�AFA�PA�\A$tA��A!�A�;A��A�A2�A��A��A�A�@Ao�A�rA�A��A4A�A��A�A2�A
��AGEAh
A8A
�A��A�DAdZAGEA�A�mAX�A�A�@AA�SA{A s�A 7L@�]�@��@�'�@���@�%�@�<6@�Ɇ@��@��~@�:�@���@��@�*0@��m@��@�'@�X�@�4@��
@���@�Y�@�Y@���@�u@�J�@�q@���@�S&@��9@�M@�+k@�qv@�f�@�_@ؾ�@��@�]d@��#@�0U@�C@Ҟ@�@�|�@�J�@�`B@�8�@�ݘ@�w�@ȇ+@��)@�(@�Q@�!�@�R�@��@���@���@�PH@�@�1@��@���@���@��4@��@���@�'�@��K@��j@�d�@���@��9@�͟@�g8@�h
@�tT@��D@��\@�4@��7@�<6@��@�5?@��"@��@�l�@�h
@���@�q@���@��n@��h@�g8@��@���@�X�@��@�Q�@��@���@���@�n�@��W@�p�@�͟@�0U@��T@��:@��4@�|�@�o @�@�_�@�@��D@��	@�+@�s�@���@�{J@�
=@��P@���@�!�@��@��@���@��@���@���@��f@�o�@�iD@�+@��@�W?@���@�y>@��@�J�@�$t@�+@��|@�ߤ@��'@��u@�g8@��@�E9@��/@���@�i�@��9@���@��@���@��@�a|@�8�@���@�خ@��F@���@�T�@�(�@��|@��.@���@���@��@���@�K^@�~@�@�@�C-@��6@�L�@���@�oi@��@�e�@���@���@�4n@��@���@�t�@�RT@��@���@�ѷ@�~(@�B[@��@��m@���@�@O@�7L@�)_@��@��f@��y@�͟@��r@�v�@�`�@�A�@�*�@��@�@�F@�4@K�@~�2@}<6@|��@|�[@|Ɇ@||�@{��@{��@{��@{��@{�:@{dZ@{+@{S@z�!@y�@y?}@x�p@x|�@x"h@x  @wخ@w��@w��@w��@w|�@wY@v�M@v�h@vE�@v-@u�o@u}�@uL�@t~@sdZ@r�!@q��@qF@p��@p��@pq@pg8@pN�@p(�@o��@ol�@o(@n��@n��@n��@np;@nOv@nu@mIR@l�@l�u@lq@l�@k��@jR�@h�4@g��@g�@f��@e}�@d��@dS�@d'R@d"h@db@c�@c�w@ce�@b�+@a�M@a4@`�?@`�I@`��@`tT@`(�@_\)@^W�@^+k@^�@^�@^ �@]�o@]��@^@^J@^J@]@]��@]f�@]=�@]�@\�5@\�@\�/@\z�@\M@\%�@\1@[��@[��@[��@[��@[�Q@[�6@[�F@[�q@[�@[��@[dZ@Z�y@XK^@Wo�@V�8@VV@U�C@U=�@T�v@T�o@TU2@T�@T�@S��@S]�@R�,@RQ@R�@Q��@P��@O��@O�	@OS�@O>�@O@O@O(@Nߤ@N�R@N��@NZ�@M��@M:�@L'R@Lx@K�@K��@K�@Kb�@KC�@K�@K
=@J�"@J��@J��@J��@Jz@J=q@J	@I�T@I��@Izx@H�/@G�;@GJ#@Fz@F-@E�D@E8�@C˒@C�q@C�4@Cs@Cl�@CW?@CH�@C/�@C�@C i@B͟@B��@A�o@A�~@AY�@A<6@A�@@�@@�@@~(@@y>@@q@@?�@?�+@?�w@?s@?E9@>�@>��@>5?@=�>@=��@=��@=x�@=�@<��@<��@;�@;��@;�:@;b�@;'�@:�,@:��@:�x@:�@:��@:~�@:	@9&�@8��@8�@8�_@8e�@8Q�@8C-@87@7�0@7��@7�@6�h@6��@6i�@6=q@5�D@5Dg@3��@2�2@2��@2H�@2�@1�>@1��@1��@1(�@0��@0��@0��@0m�@0[�@0Z@0Z@0Q�@0D�@0C-@0-�@0	�@/�@/��@/�@/qv@/l�@/n/@/9�@/1�@/�@.�"@.�@.ں@.�x@.��@.\�@.#:@.@-��@-ԕ@-��@-}�@-m]@-N<@-Dg@-4@,�@, �@+��@+�+@+�m@+� @+��@+��@+�$@+��@+x@+j�@+g�@+Z�@+>�@+9�@+)_@+o@*�@*p;@*6�@*J@)ϫ@)�n@)s�@)IR@)+@(�K@(�j@(M@'�$@'a@';d@&�}@&5?@%�Z@%��@%f�@%+�@%�@%�@$��@$ی@$��@$tT@$A�@$@#�;@#��@#�4@#o�@#]�@#H�@#!-@#�@"��@"a|@"#:@!�)@!�d@!�3@!��@!��@!��@!�S@!�S@!��@!�M@!u�@!Dg@!�@ �j@ ?�@�Q@��@��@y�@O@�@�y@��@_�@hs@��@_@S�@@��@t�@dZ@RT@4�@�@�@�@�@@�@��@� @�A@c @H�@.�@+k@�@S&@�5@�$@��@��@�O@�e@�@�4@�I@�@�Y@g8@!@��@;d@�@�8@�M@ں@�b@z@GE@	@�X@Q�@F@Dg@2a@�@�@�@�v@��@�u@��@N�@�@�@��@�q@��@�	@b�@
=@��@��@ں@ں@�6@s�@a|@J�@:*@	@�@ϫ@��@�@}�@X@F@V@g8@`�@I�@:�@6@%�@'R@7@  @��@�@�
@ƨ@��@��@_p@J#@�@��@��@�@��@��@u%@M�@�@�Z@��@�@��@��@��@�n@hs@+@�	@��@<�@��@�@��@�Q@� @�a@��@�k@�$@�P@��@��@qv@'�@
�x@	��@	+�@�)@�9@��@��@��@z�@z�@u�@j@C-@-�@�A@خ@˒@��@��@�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��MA�zxA�n�A�ncA�f�A�d�A�o�A�tTA�q�A�lWA�Z�A�u�A���A�s�A�T,A�]�A�`�A�`�A�~A���A��A�H�A�n�A�$A���A��$A��:A��MA���A���A��SA���A��(A��A��~A���A���A��PA���A��JA��JA���A��{A��A���A��A�Q�A���A�IRA��A�\]A���A�`vA�+kA���A���A�LdA��A��PA��A�v`A��sA�MjA��;A�(XA��A��A�4�A��RA��.A��A��A�	A�5�A��A�.A� 'A��A��7A��wA���A�($A�#A���A�~�A�\]A��A��A��A���A��A�^5A�uA��A�$tA�b�A�u�A��A��AԕA�AP�A~�A}��A|�oA{|�AzkQAyc�Aw[WAv($At�#Ar�Ar�Aq��Ap�TAp	An�}AmH�Al\)AkA Ah�2AeR�AaԕA^u�A]˒A]��A]�A\��A\RTA\qA[��A[a�AZ�.AZC-AX��AW�rAVYAT�?AT��AT-�AS�`AR��AO֡AJ�/AI�RAI:�AH��AH,�AG&�AFb�AE��AEH�AD��AD�AD�:ADjAD6�AC��AC�XACc�AB�AB��AB�}AB�AB�+AB_pABQAA��AA�AA��AAs�AAS&AA$�A@��A@A A?҉A?�A=�SA;oiA9��A8	lA7_A71'A7�A6��A6a�A5i�A41'A2e,A0�A/�kA/�A-��A-oA+��A)XyA(��A(b�A(0UA'��A'͟A'�~A'�A&�FA&XA&4A%w2A%<6A%!-A%�A$��A$��A$0�A#�kA#�A"��A"�A">�A ��A��A�AFA�PA�\A$tA��A!�A�;A��A�A2�A��A��A�A�@Ao�A�rA�A��A4A�A��A�A2�A
��AGEAh
A8A
�A��A�DAdZAGEA�A�mAX�A�A�@AA�SA{A s�A 7L@�]�@��@�'�@���@�%�@�<6@�Ɇ@��@��~@�:�@���@��@�*0@��m@��@�'@�X�@�4@��
@���@�Y�@�Y@���@�u@�J�@�q@���@�S&@��9@�M@�+k@�qv@�f�@�_@ؾ�@��@�]d@��#@�0U@�C@Ҟ@�@�|�@�J�@�`B@�8�@�ݘ@�w�@ȇ+@��)@�(@�Q@�!�@�R�@��@���@���@�PH@�@�1@��@���@���@��4@��@���@�'�@��K@��j@�d�@���@��9@�͟@�g8@�h
@�tT@��D@��\@�4@��7@�<6@��@�5?@��"@��@�l�@�h
@���@�q@���@��n@��h@�g8@��@���@�X�@��@�Q�@��@���@���@�n�@��W@�p�@�͟@�0U@��T@��:@��4@�|�@�o @�@�_�@�@��D@��	@�+@�s�@���@�{J@�
=@��P@���@�!�@��@��@���@��@���@���@��f@�o�@�iD@�+@��@�W?@���@�y>@��@�J�@�$t@�+@��|@�ߤ@��'@��u@�g8@��@�E9@��/@���@�i�@��9@���@��@���@��@�a|@�8�@���@�خ@��F@���@�T�@�(�@��|@��.@���@���@��@���@�K^@�~@�@�@�C-@��6@�L�@���@�oi@��@�e�@���@���@�4n@��@���@�t�@�RT@��@���@�ѷ@�~(@�B[@��@��m@���@�@O@�7L@�)_@��@��f@��y@�͟@��r@�v�@�`�@�A�@�*�@��@�@�F@�4@K�@~�2@}<6@|��@|�[@|Ɇ@||�@{��@{��@{��@{��@{�:@{dZ@{+@{S@z�!@y�@y?}@x�p@x|�@x"h@x  @wخ@w��@w��@w��@w|�@wY@v�M@v�h@vE�@v-@u�o@u}�@uL�@t~@sdZ@r�!@q��@qF@p��@p��@pq@pg8@pN�@p(�@o��@ol�@o(@n��@n��@n��@np;@nOv@nu@mIR@l�@l�u@lq@l�@k��@jR�@h�4@g��@g�@f��@e}�@d��@dS�@d'R@d"h@db@c�@c�w@ce�@b�+@a�M@a4@`�?@`�I@`��@`tT@`(�@_\)@^W�@^+k@^�@^�@^ �@]�o@]��@^@^J@^J@]@]��@]f�@]=�@]�@\�5@\�@\�/@\z�@\M@\%�@\1@[��@[��@[��@[��@[�Q@[�6@[�F@[�q@[�@[��@[dZ@Z�y@XK^@Wo�@V�8@VV@U�C@U=�@T�v@T�o@TU2@T�@T�@S��@S]�@R�,@RQ@R�@Q��@P��@O��@O�	@OS�@O>�@O@O@O(@Nߤ@N�R@N��@NZ�@M��@M:�@L'R@Lx@K�@K��@K�@Kb�@KC�@K�@K
=@J�"@J��@J��@J��@Jz@J=q@J	@I�T@I��@Izx@H�/@G�;@GJ#@Fz@F-@E�D@E8�@C˒@C�q@C�4@Cs@Cl�@CW?@CH�@C/�@C�@C i@B͟@B��@A�o@A�~@AY�@A<6@A�@@�@@�@@~(@@y>@@q@@?�@?�+@?�w@?s@?E9@>�@>��@>5?@=�>@=��@=��@=x�@=�@<��@<��@;�@;��@;�:@;b�@;'�@:�,@:��@:�x@:�@:��@:~�@:	@9&�@8��@8�@8�_@8e�@8Q�@8C-@87@7�0@7��@7�@6�h@6��@6i�@6=q@5�D@5Dg@3��@2�2@2��@2H�@2�@1�>@1��@1��@1(�@0��@0��@0��@0m�@0[�@0Z@0Z@0Q�@0D�@0C-@0-�@0	�@/�@/��@/�@/qv@/l�@/n/@/9�@/1�@/�@.�"@.�@.ں@.�x@.��@.\�@.#:@.@-��@-ԕ@-��@-}�@-m]@-N<@-Dg@-4@,�@, �@+��@+�+@+�m@+� @+��@+��@+�$@+��@+x@+j�@+g�@+Z�@+>�@+9�@+)_@+o@*�@*p;@*6�@*J@)ϫ@)�n@)s�@)IR@)+@(�K@(�j@(M@'�$@'a@';d@&�}@&5?@%�Z@%��@%f�@%+�@%�@%�@$��@$ی@$��@$tT@$A�@$@#�;@#��@#�4@#o�@#]�@#H�@#!-@#�@"��@"a|@"#:@!�)@!�d@!�3@!��@!��@!��@!�S@!�S@!��@!�M@!u�@!Dg@!�@ �j@ ?�@�Q@��@��@y�@O@�@�y@��@_�@hs@��@_@S�@@��@t�@dZ@RT@4�@�@�@�@�@@�@��@� @�A@c @H�@.�@+k@�@S&@�5@�$@��@��@�O@�e@�@�4@�I@�@�Y@g8@!@��@;d@�@�8@�M@ں@�b@z@GE@	@�X@Q�@F@Dg@2a@�@�@�@�v@��@�u@��@N�@�@�@��@�q@��@�	@b�@
=@��@��@ں@ں@�6@s�@a|@J�@:*@	@�@ϫ@��@�@}�@X@F@V@g8@`�@I�@:�@6@%�@'R@7@  @��@�@�
@ƨ@��@��@_p@J#@�@��@��@�@��@��@u%@M�@�@�Z@��@�@��@��@��@�n@hs@+@�	@��@<�@��@�@��@�Q@� @�a@��@�k@�$@�P@��@��@qv@'�@
�x@	��@	+�@�)@�9@��@��@��@z�@z�@u�@j@C-@-�@�A@خ@˒@��@��@�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B>]B>BB>]B>�B>]B>BB>�B>BB>�B>�B>�B>�B>�B>�B>]B?�B?.B?B>�B?B>�B?B?HB@ B>]B>BB>BB=�B>(B>wB>wB>�B>�B>BB>B=�B=�B=�B=�B=�B=�B=�B=<B<PB=qB;�B:*B5tB�/BxB]/BR:B:DB49B2aB.IB'�BxBPB��B�B�hB�!B�:B��B�B�B�B�B	B�BרB��B}BX�B7LB%`B4B(B�B iB�5B�$B�#B��BΥB�B��B�B��BqABaHBZ�BTFBN�BH1B@�B1�B$B.B�B	�B1BMB
��B
��B
��B
��B
�hB
�[B
ɺB
�B
��B
��B
��B
�yB
��B
��B
��B
��B
�B
x�B
mCB
S�B
A�B
<B
:xB
8�B
6`B
5B
3�B
2|B
0�B
-�B
+�B
(�B
#B
�B
�B
�B
[B
�B
DB
�B	�B	�B	�bB	�jB	ܬB	׍B	�FB	��B	ϑB	��B	�B	˒B	�)B	�	B	�B	ǮB	��B	ŢB	ðB	�B	�aB	�B	�[B	�;B	��B	�]B	�]B	��B	�VB	��B	�0B	�$B	�FB	�AB	��B	�tB	�CB	�$B	��B	�B	��B	�vB	��B	��B	�B	}B	s�B	oOB	l�B	iB	c:B	_�B	X_B	R�B	Q�B	QB	O�B	N�B	NVB	L�B	J�B	IB	HfB	F�B	EB	D�B	C�B	C-B	A�B	@B	>�B	;B	:DB	8�B	7B	1�B	+6B	'B	�B	~B	)B	7B	B	
B	�B	�B	�B	�B	�B	oB�B��B��B��B��B��B�2B�B�?B�B��B�B�B��B�B��B��B�B�2B�`B�tB�`B��B�B�bB�HB�\B�B�VB�OB�OBݲBܬBܬB�]B��B�qBۦB�QBٴB�B�1BרB�$B��B��B��B��B��B��B׍B�sB׍B�$BևB��B�B�qB��B��B�B�KB�yB�EB�_B�KB�QB�QB�IB�~B��B��B�B�pB�B�B�B��B�B�B�kB��B��B�B�B�hB��B��B�+B�DB��B�B��B�B�B��B�cB�}B	 OB	 �B	�B	�B	�B	
�B	
�B	
�B	
�B	
�B	�B	�B	PB	VB	hB	@B	MB	MB	sB	 BB	!-B	"�B	'8B	*0B	*�B	+�B	/iB	5?B	9�B	="B	>wB	>�B	?.B	?�B	A�B	CGB	E�B	G�B	HfB	J	B	JXB	J�B	KDB	M�B	P�B	QhB	Q�B	TFB	W
B	[#B	]�B	`'B	bB	a�B	c B	eB	gB	i�B	k�B	o�B	p�B	qvB	q�B	raB	rB	tB	zxB	~B	�B	�[B	�tB	�#B	�^B	�XB	�B	�B	�~B	�PB	�"B	�B	��B	��B	��B	�1B	��B	�B	�NB	��B	��B	��B	��B	��B	��B	�6B	�=B	�wB	�/B	�iB	�B	�B	��B	��B	�B	��B	�'B	ĶB	�B	ϫB	ңB	�B	�B	�xB	��B	�tB	�mB	�6B	�B	�B	�B	�B	��B	��B	��B	�LB	�rB	��B	��B	�]B
;B
�B
MB
�B
�B
�B
�B
�B
	�B

XB
B
�B
�B
�B
"B
�B
�B
HB
�B
_B
eB
�B
�B
#B
�B
jB
�B
�B
�B
�B
 \B
 �B
!�B
%zB
'mB
)B
*0B
+�B
+�B
,qB
,�B
,�B
-CB
-�B
/OB
/�B
0oB
1�B
2-B
2�B
3�B
3�B
8�B
<B
?.B
B�B
DgB
FYB
GB
GEB
G_B
G�B
H1B
IB
J�B
K�B
L�B
MjB
M�B
N"B
N�B
PB
SB
T�B
UgB
U�B
W�B
YB
]�B
a|B
d�B
gmB
h�B
l=B
o�B
r-B
r�B
r�B
sMB
tB
t�B
vB
y$B
{B
{dB
{dB
{�B
{�B
{�B
|�B
~�B
��B
��B
�[B
��B
��B
��B
��B
��B
��B
�tB
��B
�fB
��B
�=B
�DB
�^B
�^B
�xB
�~B
�B
��B
�"B
�<B
��B
��B
��B
��B
�B
�\B
��B
�\B
��B
�B
��B
�B
�WB
��B
�jB
�B
�HB
�NB
� B
��B
�&B
�&B
�tB
��B
�mB
��B
�*B
�B
�CB
� B
�OB
��B
�!B
��B
�oB
��B
�'B
�vB
�B
��B
��B
��B
��B
��B
��B
�B
��B
�	B
�XB
��B
��B
��B
��B
��B
�^B
��B
�JB
��B
��B
�6B
�VB
�HB
�4B
�B
�uB
B
āB
�_B
�_B
��B
��B
��B
�B
�B
�KB
ȚB
ȚB
��B
�RB
ʌB
��B
�DB
�^B
˒B
�~B
̘B
̳B
̘B
̳B
�B
�jB
��B
�VB
�pB
�B
��B
�bB
��B
�B
�B
�hB
��B
� B
ҽB
�,B
ԯB
��B
�MB
յB
ևB
�
B
�?B
�YB
׍B
��B
��B
�kB
��B
�#B
ۦB
�CB
�]B
�xB
��B
�dB
ݲB
�OB
��B
�;B
�;B
�VB
�VB
��B
�B
��B
�ZB
�B
��B
��B
�,B
�,B
��B
�2B
�LB
�LB
�fB
�B
�B
�fB
�B
�B
�B
�B
��B
�B
�RB
�B
��B
�B
�B
�
B
�>B
�XB
�XB
�B
�B
��B
��B
�DB
�B
�B
��B
��B
�eB
�eB
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
�CB
�wB
�]B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
�OB
�B
��B
�;B
��B
��B
�'B
�[B
��B
�-B
�3B
�B
�9B
�9B
�B
�tB
��B
�+B
�zB
��B
�B
�B
�2B
�LB
�fB
��B
�B
�RB
��B
�$B
�$B
�XB
�rB
��B
��B
��B
��B
�B
��B
�PB
��B
��B
��B
��B
��B
��B
��B
�B
�"B
�"B
��B
�B
�wB
�HB
��B �B �B �B B�B�B[BAB�BgBgB3B�BB9BSBSB�B�B�B�B�B�B�BYBYBtB�B�B�B�B�B�BKB�B�B�B�B�B�B�B�B�B	B	RB	�B
rB
�B)BDB)BxB�B�B0B�BB�B�B�B�BB"BBVB�B�B�BBBB�B�B�B�B.BHB�B B�B B�B4B�B�B�B�BBTBoB�B�B�BB�BuBB,BaBaBFB�B{B�B�B�B{B�B�B�B�BMB�BBBBBSBSBmB�B�B
B$B$B$B?B?BsB�BEByB�B�B�BBBB7B7BkBkBQB�BkBQBkB�B�BBB�B�B�B�B�B�BB�B�BpBpB�B�B�B 'B BB B44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B>]B>BB>]B>�B>]B>BB>�B>BB>�B>�B>�B>�B>�B>�B>]B?�B?.B?B>�B?B>�B?B?HB@ B>]B>BB>BB=�B>(B>wB>wB>�B>�B>BB>B=�B=�B=�B=�B=�B=�B=�B=<B<PB=qB;�B:*B5tB�/BxB]/BR:B:DB49B2aB.IB'�BxBPB��B�B�hB�!B�:B��B�B�B�B�B	B�BרB��B}BX�B7LB%`B4B(B�B iB�5B�$B�#B��BΥB�B��B�B��BqABaHBZ�BTFBN�BH1B@�B1�B$B.B�B	�B1BMB
��B
��B
��B
��B
�hB
�[B
ɺB
�B
��B
��B
��B
�yB
��B
��B
��B
��B
�B
x�B
mCB
S�B
A�B
<B
:xB
8�B
6`B
5B
3�B
2|B
0�B
-�B
+�B
(�B
#B
�B
�B
�B
[B
�B
DB
�B	�B	�B	�bB	�jB	ܬB	׍B	�FB	��B	ϑB	��B	�B	˒B	�)B	�	B	�B	ǮB	��B	ŢB	ðB	�B	�aB	�B	�[B	�;B	��B	�]B	�]B	��B	�VB	��B	�0B	�$B	�FB	�AB	��B	�tB	�CB	�$B	��B	�B	��B	�vB	��B	��B	�B	}B	s�B	oOB	l�B	iB	c:B	_�B	X_B	R�B	Q�B	QB	O�B	N�B	NVB	L�B	J�B	IB	HfB	F�B	EB	D�B	C�B	C-B	A�B	@B	>�B	;B	:DB	8�B	7B	1�B	+6B	'B	�B	~B	)B	7B	B	
B	�B	�B	�B	�B	�B	oB�B��B��B��B��B��B�2B�B�?B�B��B�B�B��B�B��B��B�B�2B�`B�tB�`B��B�B�bB�HB�\B�B�VB�OB�OBݲBܬBܬB�]B��B�qBۦB�QBٴB�B�1BרB�$B��B��B��B��B��B��B׍B�sB׍B�$BևB��B�B�qB��B��B�B�KB�yB�EB�_B�KB�QB�QB�IB�~B��B��B�B�pB�B�B�B��B�B�B�kB��B��B�B�B�hB��B��B�+B�DB��B�B��B�B�B��B�cB�}B	 OB	 �B	�B	�B	�B	
�B	
�B	
�B	
�B	
�B	�B	�B	PB	VB	hB	@B	MB	MB	sB	 BB	!-B	"�B	'8B	*0B	*�B	+�B	/iB	5?B	9�B	="B	>wB	>�B	?.B	?�B	A�B	CGB	E�B	G�B	HfB	J	B	JXB	J�B	KDB	M�B	P�B	QhB	Q�B	TFB	W
B	[#B	]�B	`'B	bB	a�B	c B	eB	gB	i�B	k�B	o�B	p�B	qvB	q�B	raB	rB	tB	zxB	~B	�B	�[B	�tB	�#B	�^B	�XB	�B	�B	�~B	�PB	�"B	�B	��B	��B	��B	�1B	��B	�B	�NB	��B	��B	��B	��B	��B	��B	�6B	�=B	�wB	�/B	�iB	�B	�B	��B	��B	�B	��B	�'B	ĶB	�B	ϫB	ңB	�B	�B	�xB	��B	�tB	�mB	�6B	�B	�B	�B	�B	��B	��B	��B	�LB	�rB	��B	��B	�]B
;B
�B
MB
�B
�B
�B
�B
�B
	�B

XB
B
�B
�B
�B
"B
�B
�B
HB
�B
_B
eB
�B
�B
#B
�B
jB
�B
�B
�B
�B
 \B
 �B
!�B
%zB
'mB
)B
*0B
+�B
+�B
,qB
,�B
,�B
-CB
-�B
/OB
/�B
0oB
1�B
2-B
2�B
3�B
3�B
8�B
<B
?.B
B�B
DgB
FYB
GB
GEB
G_B
G�B
H1B
IB
J�B
K�B
L�B
MjB
M�B
N"B
N�B
PB
SB
T�B
UgB
U�B
W�B
YB
]�B
a|B
d�B
gmB
h�B
l=B
o�B
r-B
r�B
r�B
sMB
tB
t�B
vB
y$B
{B
{dB
{dB
{�B
{�B
{�B
|�B
~�B
��B
��B
�[B
��B
��B
��B
��B
��B
��B
�tB
��B
�fB
��B
�=B
�DB
�^B
�^B
�xB
�~B
�B
��B
�"B
�<B
��B
��B
��B
��B
�B
�\B
��B
�\B
��B
�B
��B
�B
�WB
��B
�jB
�B
�HB
�NB
� B
��B
�&B
�&B
�tB
��B
�mB
��B
�*B
�B
�CB
� B
�OB
��B
�!B
��B
�oB
��B
�'B
�vB
�B
��B
��B
��B
��B
��B
��B
�B
��B
�	B
�XB
��B
��B
��B
��B
��B
�^B
��B
�JB
��B
��B
�6B
�VB
�HB
�4B
�B
�uB
B
āB
�_B
�_B
��B
��B
��B
�B
�B
�KB
ȚB
ȚB
��B
�RB
ʌB
��B
�DB
�^B
˒B
�~B
̘B
̳B
̘B
̳B
�B
�jB
��B
�VB
�pB
�B
��B
�bB
��B
�B
�B
�hB
��B
� B
ҽB
�,B
ԯB
��B
�MB
յB
ևB
�
B
�?B
�YB
׍B
��B
��B
�kB
��B
�#B
ۦB
�CB
�]B
�xB
��B
�dB
ݲB
�OB
��B
�;B
�;B
�VB
�VB
��B
�B
��B
�ZB
�B
��B
��B
�,B
�,B
��B
�2B
�LB
�LB
�fB
�B
�B
�fB
�B
�B
�B
�B
��B
�B
�RB
�B
��B
�B
�B
�
B
�>B
�XB
�XB
�B
�B
��B
��B
�DB
�B
�B
��B
��B
�eB
�eB
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
�CB
�wB
�]B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
�OB
�B
��B
�;B
��B
��B
�'B
�[B
��B
�-B
�3B
�B
�9B
�9B
�B
�tB
��B
�+B
�zB
��B
�B
�B
�2B
�LB
�fB
��B
�B
�RB
��B
�$B
�$B
�XB
�rB
��B
��B
��B
��B
�B
��B
�PB
��B
��B
��B
��B
��B
��B
��B
�B
�"B
�"B
��B
�B
�wB
�HB
��B �B �B �B B�B�B[BAB�BgBgB3B�BB9BSBSB�B�B�B�B�B�B�BYBYBtB�B�B�B�B�B�BKB�B�B�B�B�B�B�B�B�B	B	RB	�B
rB
�B)BDB)BxB�B�B0B�BB�B�B�B�BB"BBVB�B�B�BBBB�B�B�B�B.BHB�B B�B B�B4B�B�B�B�BBTBoB�B�B�BB�BuBB,BaBaBFB�B{B�B�B�B{B�B�B�B�BMB�BBBBBSBSBmB�B�B
B$B$B$B?B?BsB�BEByB�B�B�BBBB7B7BkBkBQB�BkBQBkB�B�BBB�B�B�B�B�B�BB�B�BpBpB�B�B�B 'B BB B44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230112214103  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230112214110  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230112214111  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230112214111                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230112214111  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230112214111  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230112222556                      G�O�G�O�G�O�                