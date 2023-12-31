CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-23T00:35:31Z creation;2018-10-23T00:35:37Z conversion to V3.1;2019-12-19T07:29:48Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181023003531  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              %A   JA  I2_0576_293                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؊��o��1   @؊���@9-�C�\��d5�͞��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��3D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D��3D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A��A��A��A��A��yA��`A��TA��;A��/A��A��
A�ĜAΣ�A΋DA�?}A��/A���A͛�A�E�A�A��Aˇ+A�`BA�  A�XA�S�A�VA�|�A�`BA��A�33A���A�=qA�+A��yA�t�A�z�A�ZA��A�I�A���A���A�t�A�"�A��/A�l�A���A�K�A�XA��A��A��-A�1A�ƨA�  A��uA���A�VA�dZA��yA�x�A�C�A��A��#A�E�A���A�^5A���A���A�`BA�A��#A�v�A�9XA���A���A�1'A��A��DA��A�9XA�z�A�jA�"�A��hA�5?A���A�A��!A�VA��A�ĜA��7A�|�A�33A���A��+A�5?A���A�9XA��A~��A}�^A|^5AzI�Axv�Aw�Av1'Au�FAt  Ar$�Aql�Ap�`An��Akp�Ah�jAg��Af�+Af�Ad�yAdbAb��A`��A`�A]+AZffAY33AW�TAVbNAT�AS�AS\)AR�AR��AR �AP�9AO
=AM\)AK%AJ1'AJAIdZAH�jAHZAGS�AEl�ADĜAD�uAD{AC��AB�\AAC�A?�
A>~�A=&�A<  A:��A9�A7�
A6  A3�mA3�A1�A1
=A0r�A.�`A.JA-�A-�A-x�A-K�A-/A-VA,�HA,M�A+�;A*-A)?}A)A(�!A'`BA&�\A&n�A&�A%x�A$ĜA$-A#��A#`BA"ZA!��A �yA {Ap�A�A(�A��AO�A�A1AXA�uA�#AC�AĜAI�A�HAĜA��Az�A  A�A
=A{A�!An�AVA�^A��A��A-A�AVA	�-A`BA �A�;A�-At�A7LA�yA�A�A�AVAbA ��@��@��@��m@�S�@�33@��@�x�@���@�^5@��y@��@�h@�@��@�ȴ@��@�  @�l�@�
=@���@��@�%@ߝ�@�ff@ݲ-@�bN@�t�@��@ى7@�hs@�7L@���@�  @�|�@�+@ְ!@��`@���@�\)@���@ѡ�@�1'@ΰ!@�x�@�V@�Q�@˅@��@ʟ�@��@��@ǍP@Ɨ�@�~�@�^5@ź^@�G�@�9X@���@��@��9@��w@���@�/@��@��@��@��j@��@�ƨ@���@��T@��h@�X@�?}@�bN@��@�{@���@�C�@��\@�M�@���@�Ĝ@�r�@��;@��!@�n�@�M�@���@�G�@���@��j@��@�S�@�=q@��-@���@�Z@�C�@���@�%@�(�@���@���@�-@��@�@���@��@�7L@� �@�@���@�X@�z�@��@�33@�ȴ@�~�@�^5@�J@��-@��/@�A�@�ƨ@�K�@�33@�o@��@���@���@��!@�ff@�{@��@�A�@��@�K�@��!@��+@�-@�J@��@���@�x�@�?}@�Ĝ@��@�z�@�Z@�(�@�(�@�1'@���@��w@�ƨ@���@�t�@��@��\@�$�@�$�@�V@�O�@��@��F@�|�@�S�@��@��!@��!@���@��+@�~�@�v�@�n�@�ff@��-@�V@��@�v�@��#@�X@��@���@��F@�\)@�;d@�ff@���@�?}@��@��@�Q�@�@~�R@~�y@��@�1@\)@}��@}�h@~{@~{@~{@~5?@~5?@~5?@~{@}�@}�T@}��@}�h@}`B@}�@}��@}p�@|��@{ƨ@z^5@yG�@y��@z-@z�@y�7@xĜ@w|�@v�y@v�R@vff@vv�@w
=@w\)@w;d@v��@v�@v�y@v�y@v��@vE�@u�@u��@u@u@u�-@uO�@t�/@tz�@t1@s"�@r-@q��@q7L@pĜ@pQ�@o�w@o;d@o+@o�@o+@n�y@n�R@n��@nv�@nE�@n{@m�T@m�-@m?}@m�@m�@m/@mV@kƨ@k"�@j��@jM�@iG�@h��@h�9@hr�@hb@g�w@g��@g|�@g
=@f��@f��@f�+@f{@e@e�@d�@d(�@c"�@b��@bn�@b^5@b-@ahs@a&�@a%@`�u@` �@_�;@_l�@^�@]�h@\�@\��@\��@\�j@\�j@\��@\9X@[��@[�
@[ƨ@[��@ZJ@Y7L@X�`@X�u@X�@X�@XQ�@W\)@VE�@V@U`B@T�@Tz�@TZ@T�@S�F@St�@So@R��@R�!@R�!@R�\@R�@Q�7@Qx�@QX@Q%@P��@P  @Ol�@O+@O+@O;d@O;d@Ol�@O��@O|�@O+@N��@N�R@NV@N5?@N{@N@M�@M��@M@M�-@Mp�@MO�@M�@L�@K�
@J=q@J�@I�@G�P@Gl�@G\)@GK�@GK�@G;d@G+@G+@F�y@F��@FE�@F@E�T@E�-@E�h@E�@EO�@D��@D��@Dz�@Dz�@D�D@D�D@Dz�@Dj@C�
@C��@C�F@C��@C��@C��@C��@C��@C��@C��@C��@C��@C��@C��@C�F@C��@C��@C��@C�F@C��@C�@CS�@CC�@CC�@Co@B��@B^5@B^5@B=q@B=q@A�7@Ahs@AG�@A�@@Ĝ@@�u@@bN@@Q�@@Q�@@A�@@A�@@ �@@b@@b@@  @?|�@?�@>��@>�@>��@>E�@=p�@<Z@<1@;��@;�m@;ƨ@;��@;�@;�@;t�@;S�@;S�@;dZ@;t�@;t�@;dZ@;C�@;"�@:�@:�!@:�\@:~�@:~�@:~�@:n�@:^5@:=q@9��@9��@9��@9X@9�@8�`@8Ĝ@8��@8bN@7�@7K�@6��@6E�@5�T@5@5�@4��@4j@3�m@3C�@3@2~�@2~�@2~�@2n�@2�@1��@1��@1�@1hs@17L@0��@0A�@/|�@/;d@/�@.�R@.v�@.5?@.{@.@-�T@-@-@-�-@-�-@-��@-�@-O�@,�j@,(�@+��@*��@*n�@)�#@)x�@)�@(��@(�@(r�@(bN@(1'@(  @'��@'�w@'�@'l�@'K�@';d@'�@'
=@&�@&��@&ff@%�T@%��@%�@$9X@#��@#��@#��@#��@#��@#�m@#�m@#ƨ@#��@#��@#��@#dZ@#S�@#C�@#33@"�H@"=q@"=q@"�@!��@ �@\)@�@�@v�@E�@$�@�@��@��@?}@V@��@I�@�
@C�@@�H@��@�!@^5@J@��@�#@��@�^@��@��@�7@hs@hs@hs@hs@hs@hs@hs@x�@X@�@��@��@Ĝ@��@A�@A�@A�@  @�w@��@l�@;d@;d@+@+@+@��@��@��@�y@ȴ@�R@��@�+@v�@ff@V@$�@{@$�@@�@@{@@�@@��@O�@�@z�@�D@z�@�D@j@9X@I�@I�@9X@I�@9X@(�@�@1@�m@�m@�m@��@�m@��@�@t�@C�@@��@��@�\@�#@�7@��@r�@Q�@Q�@  @��@
=@�+@ff@�@��@I�@�m@"�@
��@
��@
�\@
�!@
�\@
~�@
~�@
~�@
^5@
M�@
-@	�^@	hs@	G�@	7L@	&�@	&�@	%@�`@��@b@��@+@$�@�@p�@/@/@?}@p�@p�@��@�j@�j@�@�@Z@��@�@t�@t�@dZ@S�@dZ@S�@S�@S�@C�@33@"�@"�@��@~�@^5@=q@�#@x�@7L@ �9@ �@ 1'@ b?���?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A��A��A��A��A��yA��`A��TA��;A��/A��A��
A�ĜAΣ�A΋DA�?}A��/A���A͛�A�E�A�A��Aˇ+A�`BA�  A�XA�S�A�VA�|�A�`BA��A�33A���A�=qA�+A��yA�t�A�z�A�ZA��A�I�A���A���A�t�A�"�A��/A�l�A���A�K�A�XA��A��A��-A�1A�ƨA�  A��uA���A�VA�dZA��yA�x�A�C�A��A��#A�E�A���A�^5A���A���A�`BA�A��#A�v�A�9XA���A���A�1'A��A��DA��A�9XA�z�A�jA�"�A��hA�5?A���A�A��!A�VA��A�ĜA��7A�|�A�33A���A��+A�5?A���A�9XA��A~��A}�^A|^5AzI�Axv�Aw�Av1'Au�FAt  Ar$�Aql�Ap�`An��Akp�Ah�jAg��Af�+Af�Ad�yAdbAb��A`��A`�A]+AZffAY33AW�TAVbNAT�AS�AS\)AR�AR��AR �AP�9AO
=AM\)AK%AJ1'AJAIdZAH�jAHZAGS�AEl�ADĜAD�uAD{AC��AB�\AAC�A?�
A>~�A=&�A<  A:��A9�A7�
A6  A3�mA3�A1�A1
=A0r�A.�`A.JA-�A-�A-x�A-K�A-/A-VA,�HA,M�A+�;A*-A)?}A)A(�!A'`BA&�\A&n�A&�A%x�A$ĜA$-A#��A#`BA"ZA!��A �yA {Ap�A�A(�A��AO�A�A1AXA�uA�#AC�AĜAI�A�HAĜA��Az�A  A�A
=A{A�!An�AVA�^A��A��A-A�AVA	�-A`BA �A�;A�-At�A7LA�yA�A�A�AVAbA ��@��@��@��m@�S�@�33@��@�x�@���@�^5@��y@��@�h@�@��@�ȴ@��@�  @�l�@�
=@���@��@�%@ߝ�@�ff@ݲ-@�bN@�t�@��@ى7@�hs@�7L@���@�  @�|�@�+@ְ!@��`@���@�\)@���@ѡ�@�1'@ΰ!@�x�@�V@�Q�@˅@��@ʟ�@��@��@ǍP@Ɨ�@�~�@�^5@ź^@�G�@�9X@���@��@��9@��w@���@�/@��@��@��@��j@��@�ƨ@���@��T@��h@�X@�?}@�bN@��@�{@���@�C�@��\@�M�@���@�Ĝ@�r�@��;@��!@�n�@�M�@���@�G�@���@��j@��@�S�@�=q@��-@���@�Z@�C�@���@�%@�(�@���@���@�-@��@�@���@��@�7L@� �@�@���@�X@�z�@��@�33@�ȴ@�~�@�^5@�J@��-@��/@�A�@�ƨ@�K�@�33@�o@��@���@���@��!@�ff@�{@��@�A�@��@�K�@��!@��+@�-@�J@��@���@�x�@�?}@�Ĝ@��@�z�@�Z@�(�@�(�@�1'@���@��w@�ƨ@���@�t�@��@��\@�$�@�$�@�V@�O�@��@��F@�|�@�S�@��@��!@��!@���@��+@�~�@�v�@�n�@�ff@��-@�V@��@�v�@��#@�X@��@���@��F@�\)@�;d@�ff@���@�?}@��@��@�Q�@�@~�R@~�y@��@�1@\)@}��@}�h@~{@~{@~{@~5?@~5?@~5?@~{@}�@}�T@}��@}�h@}`B@}�@}��@}p�@|��@{ƨ@z^5@yG�@y��@z-@z�@y�7@xĜ@w|�@v�y@v�R@vff@vv�@w
=@w\)@w;d@v��@v�@v�y@v�y@v��@vE�@u�@u��@u@u@u�-@uO�@t�/@tz�@t1@s"�@r-@q��@q7L@pĜ@pQ�@o�w@o;d@o+@o�@o+@n�y@n�R@n��@nv�@nE�@n{@m�T@m�-@m?}@m�@m�@m/@mV@kƨ@k"�@j��@jM�@iG�@h��@h�9@hr�@hb@g�w@g��@g|�@g
=@f��@f��@f�+@f{@e@e�@d�@d(�@c"�@b��@bn�@b^5@b-@ahs@a&�@a%@`�u@` �@_�;@_l�@^�@]�h@\�@\��@\��@\�j@\�j@\��@\9X@[��@[�
@[ƨ@[��@ZJ@Y7L@X�`@X�u@X�@X�@XQ�@W\)@VE�@V@U`B@T�@Tz�@TZ@T�@S�F@St�@So@R��@R�!@R�!@R�\@R�@Q�7@Qx�@QX@Q%@P��@P  @Ol�@O+@O+@O;d@O;d@Ol�@O��@O|�@O+@N��@N�R@NV@N5?@N{@N@M�@M��@M@M�-@Mp�@MO�@M�@L�@K�
@J=q@J�@I�@G�P@Gl�@G\)@GK�@GK�@G;d@G+@G+@F�y@F��@FE�@F@E�T@E�-@E�h@E�@EO�@D��@D��@Dz�@Dz�@D�D@D�D@Dz�@Dj@C�
@C��@C�F@C��@C��@C��@C��@C��@C��@C��@C��@C��@C��@C��@C�F@C��@C��@C��@C�F@C��@C�@CS�@CC�@CC�@Co@B��@B^5@B^5@B=q@B=q@A�7@Ahs@AG�@A�@@Ĝ@@�u@@bN@@Q�@@Q�@@A�@@A�@@ �@@b@@b@@  @?|�@?�@>��@>�@>��@>E�@=p�@<Z@<1@;��@;�m@;ƨ@;��@;�@;�@;t�@;S�@;S�@;dZ@;t�@;t�@;dZ@;C�@;"�@:�@:�!@:�\@:~�@:~�@:~�@:n�@:^5@:=q@9��@9��@9��@9X@9�@8�`@8Ĝ@8��@8bN@7�@7K�@6��@6E�@5�T@5@5�@4��@4j@3�m@3C�@3@2~�@2~�@2~�@2n�@2�@1��@1��@1�@1hs@17L@0��@0A�@/|�@/;d@/�@.�R@.v�@.5?@.{@.@-�T@-@-@-�-@-�-@-��@-�@-O�@,�j@,(�@+��@*��@*n�@)�#@)x�@)�@(��@(�@(r�@(bN@(1'@(  @'��@'�w@'�@'l�@'K�@';d@'�@'
=@&�@&��@&ff@%�T@%��@%�@$9X@#��@#��@#��@#��@#��@#�m@#�m@#ƨ@#��@#��@#��@#dZ@#S�@#C�@#33@"�H@"=q@"=q@"�@!��@ �@\)@�@�@v�@E�@$�@�@��@��@?}@V@��@I�@�
@C�@@�H@��@�!@^5@J@��@�#@��@�^@��@��@�7@hs@hs@hs@hs@hs@hs@hs@x�@X@�@��@��@Ĝ@��@A�@A�@A�@  @�w@��@l�@;d@;d@+@+@+@��@��@��@�y@ȴ@�R@��@�+@v�@ff@V@$�@{@$�@@�@@{@@�@@��@O�@�@z�@�D@z�@�D@j@9X@I�@I�@9X@I�@9X@(�@�@1@�m@�m@�m@��@�m@��@�@t�@C�@@��@��@�\@�#@�7@��@r�@Q�@Q�@  @��@
=@�+@ff@�@��@I�@�m@"�@
��@
��@
�\@
�!@
�\@
~�@
~�@
~�@
^5@
M�@
-@	�^@	hs@	G�@	7L@	&�@	&�@	%@�`@��@b@��@+@$�@�@p�@/@/@?}@p�@p�@��@�j@�j@�@�@Z@��@�@t�@t�@dZ@S�@dZ@S�@S�@S�@C�@33@"�@"�@��@~�@^5@=q@�#@x�@7L@ �9@ �@ 1'@ b?���?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�ZB�)B�;B�`B�mB�sB�BB��B��B�B�B�B0!B/B�BoBPB.B	7B�B�B+BL�BG�B<jB0!B33B?}B=qBI�BI�B>wB/B�B�B�B%B��B�B�NB�BB�BB��B��BŢBŢB�dB�XB�B��B��B�{B��B�bB�1B�%B|�BbNBk�BhsBbNB^5BK�B5?B\B  B
�NB
�TB
��B
�B
�/B
�#B
��B
��B
�B
�3B
�9B
�B
�B
�B
��B
��B
��B
�uB
�1B
�B
{�B
q�B
iyB
ZB
L�B
?}B
;dB
8RB
33B
"�B
{B
{B
VB	��B	�)B	��B	�B	�B	�
B	��B	ƨB	�jB	�B	��B	�oB	{�B	�B	v�B	s�B	`BB	cTB	hsB	dZB	_;B	XB	H�B	:^B	9XB	%�B	2-B	6FB	0!B	)�B	(�B	�B	\B	�B	�B	{B	\B	B��B�B�B�mB�NB�B��BȴB��B�RB�wB�jB�?B�LB�B�B�3B�-B�!B�!B�B�B��B��B��B�{B��B��B��B��B�{B��B��B�oB�PB�PB�VB�1B~�B�B{�By�Bz�Bx�Bv�Bt�Bv�Bo�Bo�Bk�BiyBhsBhsBe`BaHBW
BH�BbNB_;BXBN�BB�BK�BG�BO�BQ�BK�BM�BE�BE�BF�B=qB0!B!�B.B=qB=qB<jB:^B7LB2-B0!B1'B0!B0!B$�B%�B,B�B.B1'B-B!�BuBVBuB$�B+B%�B%�B#�B!�B �B#�B$�B�BoB�B�B�B#�B�B!�B!�B+B-B,B(�B&�B'�B'�B%�B�B$�B%�B$�B!�B�B �B$�B,B)�B(�B+B-B(�B'�B$�B)�B2-B0!B,B+B(�B$�B)�B33B1'B2-B33B>wBC�BC�BA�B>wB:^B9XB>wBC�BC�BB�B<jB6FB?}B=qB?}BI�BL�BL�BH�BO�BO�BL�BW
BXBVBYBYB\)BYBXBZB`BB`BBaHB`BB^5Bk�Bk�Bo�Bp�Bs�Bx�Bz�Bz�By�Bw�Bs�Bu�Bz�B�B�B�1B�1B�\B�hB�uB�oB�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�qB�qBÖBĜBŢBĜBǮBɺB��B��B��B��B�B�B�B�`B�mB�mB�fB�mB�yB�B�B�B�sB�B�B��B��B��B��B	B	1B	DB	VB	VB	VB	PB		7B	
=B		7B	VB	oB	�B	�B	%�B	%�B	1'B	8RB	9XB	C�B	J�B	N�B	N�B	R�B	W
B	YB	`BB	p�B	s�B	s�B	s�B	x�B	|�B	~�B	�%B	�1B	�7B	�1B	�7B	�=B	�DB	�DB	�PB	�VB	�hB	�oB	�oB	�hB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�?B	�FB	�RB	�^B	�XB	�XB	�XB	�^B	�jB	�jB	�qB	�jB	�dB	�dB	�jB	�dB	�dB	�dB	��B	��B	B	B	ÖB	ÖB	ƨB	ƨB	ƨB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ȴB	ɺB	ɺB	ȴB	ƨB	ǮB	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�B	�)B	�/B	�)B	�/B	�5B	�/B	�5B	�/B	�ZB	�sB	�yB	�yB	�yB	�sB	�mB	�yB	�B	�yB	�sB	�ZB	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
DB
VB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
&�B
&�B
&�B
'�B
&�B
&�B
%�B
%�B
'�B
'�B
'�B
%�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
+B
)�B
+B
-B
,B
,B
+B
)�B
)�B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
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
33B
2-B
2-B
33B
5?B
5?B
6FB
5?B
49B
6FB
6FB
6FB
8RB
8RB
:^B
:^B
:^B
9XB
:^B
;dB
:^B
9XB
:^B
:^B
:^B
:^B
=qB
>wB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
?}B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
F�B
G�B
F�B
F�B
I�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
G�B
J�B
H�B
G�B
E�B
F�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
N�B
O�B
O�B
N�B
O�B
P�B
R�B
S�B
S�B
T�B
S�B
S�B
VB
VB
VB
VB
W
B
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
W
B
W
B
VB
W
B
XB
XB
XB
W
B
YB
YB
XB
XB
YB
YB
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
]/B
]/B
]/B
]/B
^5B
]/B
]/B
\)B
\)B
\)B
[#B
[#B
_;B
`BB
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
`BB
`BB
`BB
aHB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
^5B
aHB
`BB
bNB
dZB
dZB
cTB
cTB
aHB
bNB
e`B
bNB
dZB
e`B
gmB
ffB
hsB
k�B
k�B
l�B
k�B
k�B
k�B
k�B
jB
k�B
jB
iyB
jB
l�B
m�B
l�B
l�B
l�B
jB
jB
hsB
jB
iyB
gmB
n�B
n�B
p�B
r�B
r�B
r�B
q�B
o�B
p�B
r�B
q�B
q�B
p�B
o�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
t�B
s�B
u�B
u�B
w�B
v�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BߊB��B�>B�yB�hB�XB��B��B!B �B1'B1vB!B�B�B3hBhBjB%`B/�BN�BJrB?�B49B6�BA�B?�BJrBJrB?�B1[B!�B �B!|B
#B�JB�'B�B��B�B��B�(B�+BƨB�jB�*B��B�jB�-B��B�B�NB�B��B~Bd�BlWBiBcB^�BM�B7�B�B�B
�B
��B
�+B
�cB
ޞB
�B
�NB
��B
�iB
�B
��B
��B
��B
�QB
��B
��B
�KB
�aB
��B
�'B
}B
r�B
j�B
\B
OBB
A�B
<�B
9rB
49B
$�B
�B
�B
�B	��B	��B	��B	�kB	�YB	��B	�\B	��B	�]B	�;B	��B	��B	HB	��B	x�B	u�B	b�B	dZB	h�B	d�B	_�B	YB	J�B	<�B	;B	(�B	3B	6�B	1B	*�B	)�B	 BB	hB	YB	�B	MB	HB	�B��B�B�WB�*B��B��B�"BʦB��B��B��B��B�zB�RB��B�B��B��B�oB�oB�iB�cB�B��B��B��B��B�LB��B�+B�gB�B�1B�[B�VB�"B��B�B�iB��B}"Bz�B{�By�Bw�Bu�BwLBp�BpoBl�BjBi�Bi_BfLBbhBX�BK)Bb�B_�BYBP�BD�BM6BIRBPHBR:BL�BNVBF�BFYBG_B>�B2GB$�B/�B=�B=�B<�B:�B8B3hB1B2B0�B1B&�B'B,�B!|B.}B1vB-�B#:B�B B�B%zB+�B&�B&�B$�B"�B!�B$ZB%`B�BaB�B �B�B$tB �B"�B"�B+QB-CB,qB)yB'�B(XB(XB&�BB%zB&fB%zB"�B�B!�B%�B,qB*�B)�B+�B-]B)�B(�B%�B*�B2GB0oB,�B+�B)�B%�B+B3�B1�B2�B4TB>�BC�BC�BA�B>�B;B:*B>�BC�BC�BB�B=<B7�B@B>�B@iBJ=BMBM6BI�BP.BPbBM�BW?BX_BVmBYeBYB\xBY�BX�BZ�B`�B`�Ba�Ba-B_VBlBl=Bp!BqABt9By	B{B{Bz*Bx8Bt�Bv�B{�B�uB��B��B��B��B��B��B��B��B� B�
B�B�!B��B�B�B�B��B�B� B�HB��B�tB�cB��B��B��B��B��B��B��B��B��B�	B��B�B� B�,B�B�+B�B�zB�B�B��B��B��B��B�B��B�DB�B�B��B��B�B�B	�B	KB	^B	pB	pB	pB	�B		�B	
�B	
XB	B	�B	�B	B	&2B	&�B	1vB	8�B	:B	DB	KB	OB	OBB	S@B	WYB	YeB	`BB	p;B	s�B	tB	t9B	x�B	|�B	~�B	�B	�1B	�7B	�KB	�RB	�=B	�^B	�^B	�jB	�pB	�hB	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	��B	�B	�B	�B	��B	�9B	�ZB	�`B	�lB	�DB	�XB	�rB	��B	�xB	��B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	ƨB	ƨB	ƨB	żB	żB	��B	ƨB	��B	ƨB	��B	��B	��B	��B	ɺB	��B	��B	�B	��B	��B	��B	�#B	��B	��B	��B	�B	� B	�B	�B	�B	�B	��B	�B	� B	�&B	�@B	�,B	�2B	�FB	�1B	�QB	�=B	�7B	�KB	�]B	�IB	�]B	�dB	�OB	�dB	ބB	ݲB	�B	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�	B	��B	�	B	��B	�B	��B	�B	��B	�B	�B	�B	��B	�B	�(B	�BB	�.B
;B
EB
DB
pB
VB
\B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
#�B
$�B
$�B
$�B
$�B
#�B
#�B
#B
'B
'�B
'B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
'B
'B
'B
'�B
'B
'B
%�B
%�B
'�B
(
B
($B
&B
(
B
)B
)B
)B
*B
+B
+B
+B
+B
+B
+B
,B
,B
+B
*0B
+B
-)B
,=B
,"B
+6B
*KB
*KB
/5B
0!B
0;B
0;B
1AB
1AB
1'B
1'B
1[B
2-B
2-B
2-B
2-B
1AB
1AB
1AB
1'B
1[B
2GB
33B
33B
33B
3B
2GB
2GB
2GB
3MB
3MB
3MB
3MB
3MB
4TB
49B
3MB
2aB
2|B
3hB
5ZB
5ZB
6`B
5�B
4�B
6zB
6zB
6zB
8RB
8�B
:xB
:^B
:xB
9XB
:xB
;dB
:xB
9�B
:xB
:�B
:�B
:�B
=�B
>�B
=�B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
?�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
F�B
G�B
G�B
F�B
G�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
F�B
G�B
F�B
F�B
I�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
G�B
J�B
H�B
G�B
E�B
F�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
OB
O�B
O�B
N�B
PB
QB
R�B
TB
TB
UB
TB
T,B
VB
VB
VB
VB
W
B
VB
VB
W
B
W
B
W
B
V�B
W
B
W
B
W
B
W
B
W$B
VB
W$B
XB
X+B
X+B
W$B
YB
YB
X+B
XEB
Y1B
Y1B
Z7B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[	B
[=B
[=B
[=B
\)B
\)B
\)B
\)B
\CB
]/B
]/B
]IB
]/B
^B
]/B
]/B
\CB
\CB
\CB
[WB
[WB
_!B
`BB
_;B
_;B
_pB
_VB
`BB
`BB
`BB
`BB
`BB
`BB
`'B
`BB
`BB
`BB
aHB
`BB
`\B
_VB
`BB
`\B
`\B
`\B
`\B
a-B
`\B
^�B
abB
`�B
bhB
dtB
dtB
cTB
cnB
a�B
b�B
ezB
b�B
d�B
ezB
g�B
f�B
h�B
k�B
k�B
l�B
k�B
kkB
k�B
k�B
j�B
k�B
j�B
i�B
j�B
l�B
m�B
l�B
l�B
l�B
jB
j�B
h�B
j�B
i�B
g�B
n�B
n�B
p�B
r�B
r�B
r�B
q�B
o�B
p�B
r�B
q�B
q�B
p�B
o�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
t�B
s�B
u�B
u�B
xB
v�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810270038042018102700380420181027003804201810270200162018102702001620181027020016201810280027282018102800272820181028002728  JA  ARFMdecpA19c                                                                20181023093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181023003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181023003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181023003534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181023003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181023003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181023003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181023003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181023003537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181023003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20181023005750                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181023153429  CV  JULD            G�O�G�O�F�W�                JM  ARCAJMQC2.0                                                                 20181026153804  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181026153804  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181026170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181027152728  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                