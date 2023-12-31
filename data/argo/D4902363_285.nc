CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-29T00:35:31Z creation;2018-09-29T00:35:36Z conversion to V3.1;2019-12-19T07:31:38Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20180929003531  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_285                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؄�+<�1   @؄��j1�@9��1&��d?��S��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aө�Aӝ�AӍPAӁA�|�A�z�A�r�A�l�A�hsA�ffA�^5A�XA�VA�Q�A�I�A�C�A�?}A�=qA�;dA�9XA�9XA�;dA�C�A�K�A�G�A�33A��A��A�1'A�G�A�v�Aʣ�A��/A�O�A���A�M�A�I�A��`A���A���A���A�(�A�A��A�dZA���A�jA���A�A�A��yA�|�A��A��DA�bA�`BA��mA�{A�S�A��
A�-A�r�A�ȴA�E�A��`A��!A�33A��+A� �A��A���A�/A���A�%A��A�|�A�ƨA���A�XA��A�XA��\A��A���A��`A���A���A��A�S�A�O�A���A�C�A��A��;A�^5A�
=A��A�ƨA�-A��A�-A��FA�-A�33A�-A���A�^5A��
A��!A�^5A��A�(�A��9A�S�A33A~9XA}��A}Az��Ay��Ay
=Av�Au%As�As
=AqK�AodZAmXAk�AjE�AiO�Ag�Ae�Ab�!Aal�A`1'A^9XA]�PA\1'A[oAZ �AZ  AY�AY�;AY�hAXĜAW�AV1'AU�ATr�AT9XASx�ARffAP�AO��AO7LANv�AMƨAMAK��AKl�AJ��AJv�AJAI�PAH�yAH$�AF��AE�AD$�AC\)ABȴAA%A@  A?%A<�yA;�A;ƨA;�A;O�A:�uA8�A5S�A4��A4(�A3`BA2��A2JA1x�A1�A0��A/�TA/�FA.��A.�\A-�A,��A,9XA+�A+��A*�A*{A);dA'XA&�\A&  A%�7A%"�A$��A#ƨA"jA"1A!O�A�-A�uAjA=qAJAp�AĜAffA  A�mA�wA��A�PA��A|�A;dA�!A�
A��A��Ar�A  A�A��A��AffAG�A�/AȴA��AVA  A��AA�A
��A	�;A��A`BA5?A��AoA5?Ap�A�AO�A ��A ^5@���@�$�@�O�@��@��w@��+@��@�@�D@�1@�l�@��@�7@��D@�w@�\@�ff@���@ꟾ@�@�%@�j@�|�@��H@�@�^@��@߮@�+@އ+@�$�@ݲ-@�`B@���@��;@ڸR@�^5@���@�%@�A�@׍P@�"�@�^5@ա�@�`B@Ԭ@�;d@�J@с@�9X@��y@���@��@�r�@˾w@�{@�Ĝ@�|�@�5?@�`B@���@��7@�/@���@���@��R@���@�  @�@���@�ff@��@��^@�x�@��9@�Q�@��
@��!@�7L@��u@��P@���@��@�G�@���@�j@�dZ@���@�O�@��/@���@��m@�v�@�/@��@�bN@�9X@��;@�t�@�K�@�C�@�K�@�+@�ȴ@��@��@���@���@���@�=q@�p�@��@���@��\@�@��@� �@�+@��@���@�E�@��@�x�@�7L@�%@���@�z�@� �@�1@�ƨ@�dZ@�;d@���@��+@�-@�@��@�b@��w@��@�dZ@�"�@���@��!@��+@�^5@�J@���@�&�@���@�9X@�;d@��y@�-@��@���@�X@�?}@��@�bN@�  @��
@��@��\@��-@�%@��@��j@���@��u@�j@�1'@��@�K�@�@��y@�ȴ@�E�@��^@���@��@��@�I�@�l�@�K�@�33@�o@���@�E�@��#@���@�x�@��@�bN@�9X@�(�@��@�@�@
=@~5?@}�@}V@|��@|��@|z�@|(�@|1@{�
@{�F@{t�@{o@z~�@z-@y�#@y�^@y��@y��@y��@y��@yhs@y&�@y&�@x��@xQ�@w�@w�w@w��@w|�@wl�@wK�@wK�@w+@v��@vV@u�T@u@up�@t�@tZ@tI�@s�m@sC�@r�@r��@r��@r��@r�@r�H@r~�@q��@qX@qX@qX@qX@q��@q��@q��@q�7@qX@pĜ@p�u@pA�@o�@o|�@o;d@o
=@n��@n�y@n�@n�@n�R@n{@mp�@l��@m�@m/@l�@l1@k�F@k��@kt�@kC�@j�\@j=q@j�@i��@ihs@i&�@hA�@g��@g�w@g��@gl�@g;d@f�y@fv�@f5?@e��@e�@d1@c�
@c�F@ct�@cC�@c@b��@b�@a�^@a��@a�7@aX@a�@`�`@`��@`��@`�@`  @^�R@]@]`B@]/@\��@\�@\�D@\�D@\��@\9X@\1@[�
@[t�@[@Z��@Z�!@Z��@Z�\@Z~�@Zn�@ZM�@Z-@Y�#@Y�7@Y7L@YG�@Y%@XQ�@Xb@W�w@W|�@V�@VE�@U?}@TI�@S��@S��@Sƨ@T1@Sƨ@St�@SS�@SC�@R�@R-@Q��@Q�7@Qx�@Q�7@Q7L@P�@Pb@O��@OK�@O
=@N�y@Nff@M��@M��@M�@M`B@M�@L��@L(�@K�m@K��@J��@J^5@I��@I��@J�@I�^@IG�@I%@HĜ@Hr�@Hb@G��@G�@F��@F��@F��@Fv�@E�@E�@E�@E�@E`B@EV@D�@D��@D�j@DZ@D(�@D1@C��@Cƨ@Ct�@CC�@C33@C"�@Co@C@B�@B��@BM�@A��@A��@AG�@A&�@A�@@��@@Ĝ@@��@@�@@b@?��@?+@>�y@>��@>ff@>E�@>E�@>{@=@=�@=V@<��@<�@<��@<�j@<�@<I�@;�
@;ƨ@;�F@;��@;��@;C�@:��@:��@:^5@:-@:J@9��@9��@9��@9��@9x�@9�@9%@8��@8��@8�9@8A�@7�w@7�P@7��@7;d@6v�@6ff@6�+@6E�@6@6{@5��@5`B@5`B@5�@5@5�T@5�@5�@5�@5�@5@5?}@4��@4��@4�/@4��@4�@4��@49X@4�@4�@3�@333@333@3o@3@2�@2��@2~�@2=q@1��@0�`@0A�@0  @/�w@/|�@/;d@/�@.ȴ@.V@.@-��@,�@,9X@,1@+�
@+t�@*~�@*J@)�#@)x�@)�@(��@(�9@(�@(A�@'�w@'l�@'�@&E�@&{@%��@%p�@%�@$�@$��@$z�@$(�@#��@#�F@#dZ@#C�@"�!@"��@"=q@!�@!7L@!�@!&�@!X@!7L@ ��@ ��@ r�@ 1'@   @�@��@�@l�@+@�@��@�y@�R@�+@�@��@�@?}@�@�D@�D@�@�
@t�@C�@33@"�@@�H@�H@�!@�\@^5@�@�#@��@��@�7@X@7L@�9@A�@b@�P@l�@l�@\)@;d@+@�R@v�@v�@V@$�@@�T@�h@`B@?}@V@��@z�@Z@ƨ@��@t�@t�@33@"�@"�@"�@@o@@�@�!@n�@�@��@��@x�@7L@�u@Q�@A�@A�@ �@�@��@��@\)@
=@�@�R@��@v�@V@$�@@`B@?}@V@�@��@�@��@Z@�@ƨ@C�@"�@o@o@
�@
��@
��@
��@
M�@
-@
J@	�#@	�#@	�^@	�^@	��@	��@	��@	��@	�7@	�7@	�7@	hs@	%@Ĝ@�9@�9@�9@�9@�9@�9@�9@�9@��@�u@�u@�@r�@A�@��@\)@;d@�@
=@��@�y@��@E�@@@5?@$�@�@��@@�h@?}@�/@�@�j@��@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aө�Aӝ�AӍPAӁA�|�A�z�A�r�A�l�A�hsA�ffA�^5A�XA�VA�Q�A�I�A�C�A�?}A�=qA�;dA�9XA�9XA�;dA�C�A�K�A�G�A�33A��A��A�1'A�G�A�v�Aʣ�A��/A�O�A���A�M�A�I�A��`A���A���A���A�(�A�A��A�dZA���A�jA���A�A�A��yA�|�A��A��DA�bA�`BA��mA�{A�S�A��
A�-A�r�A�ȴA�E�A��`A��!A�33A��+A� �A��A���A�/A���A�%A��A�|�A�ƨA���A�XA��A�XA��\A��A���A��`A���A���A��A�S�A�O�A���A�C�A��A��;A�^5A�
=A��A�ƨA�-A��A�-A��FA�-A�33A�-A���A�^5A��
A��!A�^5A��A�(�A��9A�S�A33A~9XA}��A}Az��Ay��Ay
=Av�Au%As�As
=AqK�AodZAmXAk�AjE�AiO�Ag�Ae�Ab�!Aal�A`1'A^9XA]�PA\1'A[oAZ �AZ  AY�AY�;AY�hAXĜAW�AV1'AU�ATr�AT9XASx�ARffAP�AO��AO7LANv�AMƨAMAK��AKl�AJ��AJv�AJAI�PAH�yAH$�AF��AE�AD$�AC\)ABȴAA%A@  A?%A<�yA;�A;ƨA;�A;O�A:�uA8�A5S�A4��A4(�A3`BA2��A2JA1x�A1�A0��A/�TA/�FA.��A.�\A-�A,��A,9XA+�A+��A*�A*{A);dA'XA&�\A&  A%�7A%"�A$��A#ƨA"jA"1A!O�A�-A�uAjA=qAJAp�AĜAffA  A�mA�wA��A�PA��A|�A;dA�!A�
A��A��Ar�A  A�A��A��AffAG�A�/AȴA��AVA  A��AA�A
��A	�;A��A`BA5?A��AoA5?Ap�A�AO�A ��A ^5@���@�$�@�O�@��@��w@��+@��@�@�D@�1@�l�@��@�7@��D@�w@�\@�ff@���@ꟾ@�@�%@�j@�|�@��H@�@�^@��@߮@�+@އ+@�$�@ݲ-@�`B@���@��;@ڸR@�^5@���@�%@�A�@׍P@�"�@�^5@ա�@�`B@Ԭ@�;d@�J@с@�9X@��y@���@��@�r�@˾w@�{@�Ĝ@�|�@�5?@�`B@���@��7@�/@���@���@��R@���@�  @�@���@�ff@��@��^@�x�@��9@�Q�@��
@��!@�7L@��u@��P@���@��@�G�@���@�j@�dZ@���@�O�@��/@���@��m@�v�@�/@��@�bN@�9X@��;@�t�@�K�@�C�@�K�@�+@�ȴ@��@��@���@���@���@�=q@�p�@��@���@��\@�@��@� �@�+@��@���@�E�@��@�x�@�7L@�%@���@�z�@� �@�1@�ƨ@�dZ@�;d@���@��+@�-@�@��@�b@��w@��@�dZ@�"�@���@��!@��+@�^5@�J@���@�&�@���@�9X@�;d@��y@�-@��@���@�X@�?}@��@�bN@�  @��
@��@��\@��-@�%@��@��j@���@��u@�j@�1'@��@�K�@�@��y@�ȴ@�E�@��^@���@��@��@�I�@�l�@�K�@�33@�o@���@�E�@��#@���@�x�@��@�bN@�9X@�(�@��@�@�@
=@~5?@}�@}V@|��@|��@|z�@|(�@|1@{�
@{�F@{t�@{o@z~�@z-@y�#@y�^@y��@y��@y��@y��@yhs@y&�@y&�@x��@xQ�@w�@w�w@w��@w|�@wl�@wK�@wK�@w+@v��@vV@u�T@u@up�@t�@tZ@tI�@s�m@sC�@r�@r��@r��@r��@r�@r�H@r~�@q��@qX@qX@qX@qX@q��@q��@q��@q�7@qX@pĜ@p�u@pA�@o�@o|�@o;d@o
=@n��@n�y@n�@n�@n�R@n{@mp�@l��@m�@m/@l�@l1@k�F@k��@kt�@kC�@j�\@j=q@j�@i��@ihs@i&�@hA�@g��@g�w@g��@gl�@g;d@f�y@fv�@f5?@e��@e�@d1@c�
@c�F@ct�@cC�@c@b��@b�@a�^@a��@a�7@aX@a�@`�`@`��@`��@`�@`  @^�R@]@]`B@]/@\��@\�@\�D@\�D@\��@\9X@\1@[�
@[t�@[@Z��@Z�!@Z��@Z�\@Z~�@Zn�@ZM�@Z-@Y�#@Y�7@Y7L@YG�@Y%@XQ�@Xb@W�w@W|�@V�@VE�@U?}@TI�@S��@S��@Sƨ@T1@Sƨ@St�@SS�@SC�@R�@R-@Q��@Q�7@Qx�@Q�7@Q7L@P�@Pb@O��@OK�@O
=@N�y@Nff@M��@M��@M�@M`B@M�@L��@L(�@K�m@K��@J��@J^5@I��@I��@J�@I�^@IG�@I%@HĜ@Hr�@Hb@G��@G�@F��@F��@F��@Fv�@E�@E�@E�@E�@E`B@EV@D�@D��@D�j@DZ@D(�@D1@C��@Cƨ@Ct�@CC�@C33@C"�@Co@C@B�@B��@BM�@A��@A��@AG�@A&�@A�@@��@@Ĝ@@��@@�@@b@?��@?+@>�y@>��@>ff@>E�@>E�@>{@=@=�@=V@<��@<�@<��@<�j@<�@<I�@;�
@;ƨ@;�F@;��@;��@;C�@:��@:��@:^5@:-@:J@9��@9��@9��@9��@9x�@9�@9%@8��@8��@8�9@8A�@7�w@7�P@7��@7;d@6v�@6ff@6�+@6E�@6@6{@5��@5`B@5`B@5�@5@5�T@5�@5�@5�@5�@5@5?}@4��@4��@4�/@4��@4�@4��@49X@4�@4�@3�@333@333@3o@3@2�@2��@2~�@2=q@1��@0�`@0A�@0  @/�w@/|�@/;d@/�@.ȴ@.V@.@-��@,�@,9X@,1@+�
@+t�@*~�@*J@)�#@)x�@)�@(��@(�9@(�@(A�@'�w@'l�@'�@&E�@&{@%��@%p�@%�@$�@$��@$z�@$(�@#��@#�F@#dZ@#C�@"�!@"��@"=q@!�@!7L@!�@!&�@!X@!7L@ ��@ ��@ r�@ 1'@   @�@��@�@l�@+@�@��@�y@�R@�+@�@��@�@?}@�@�D@�D@�@�
@t�@C�@33@"�@@�H@�H@�!@�\@^5@�@�#@��@��@�7@X@7L@�9@A�@b@�P@l�@l�@\)@;d@+@�R@v�@v�@V@$�@@�T@�h@`B@?}@V@��@z�@Z@ƨ@��@t�@t�@33@"�@"�@"�@@o@@�@�!@n�@�@��@��@x�@7L@�u@Q�@A�@A�@ �@�@��@��@\)@
=@�@�R@��@v�@V@$�@@`B@?}@V@�@��@�@��@Z@�@ƨ@C�@"�@o@o@
�@
��@
��@
��@
M�@
-@
J@	�#@	�#@	�^@	�^@	��@	��@	��@	��@	�7@	�7@	�7@	hs@	%@Ĝ@�9@�9@�9@�9@�9@�9@�9@�9@��@�u@�u@�@r�@A�@��@\)@;d@�@
=@��@�y@��@E�@@@5?@$�@�@��@@�h@?}@�/@�@�j@��@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BF�BH�BI�BI�BJ�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BM�BN�BN�BO�BR�BS�BW
B\)BcTBr�B�uB�qB�NB%B1'B_;B��B��B��B�PBl�BI�B\)B� B�+Bu�B�DB��Br�Bw�Bq�B�B�B�B�B}�Bt�Bk�BaHB@�B>wBJ�BW
BI�BJ�BC�BH�BH�BJ�B=qB1'B-BbBB�B�B
=B%B��B�B�B�B�B�`B�BŢB��B�B��B�1B�PBz�BjBdZBjBaHBK�BF�BF�B:^B-B�BuB
�B
��B
��B
�TB
��B
�#B
�
B
ǮB
��B
�B
�PB
ffB
�B
�B
t�B
m�B
o�B
e`B
I�B
J�B
I�B
1'B
�B
!�B
�B
B	�B	�ZB	�ZB	�B	��B	��B	��B	��B	��B	��B	�\B	��B	�VB	��B	�uB	��B	��B	��B	��B	�7B	w�B	t�B	|�B	l�B	u�B	iyB	XB	Q�B	T�B	Q�B	P�B	G�B	F�B	A�B	?}B	>wB	C�B	<jB	49B	)�B	"�B	1B	
=B��B��B�B��B��B��B��B��B�;B�B��B�wB��B�oB�-B�jB�-B�?B�!B�9B�3B�!B��B�'B��B��B��B��B��B��B��B��B�PB�JBx�B�hB�uB�oB�hB�=B~�Bu�B~�By�BhsBl�B�B�B|�Bt�Bt�Bv�By�B|�Bz�Bv�Bs�BffBT�BffB^5BQ�BK�BJ�BG�BQ�BVBD�B=qB@�BA�BN�BW
BS�BO�BI�BC�B0!B$�B1'B#�B)�B&�B9XB,B(�B)�B�B+B/B.B-B%�B+B-B'�B!�B�BhB �B(�B'�B#�B�B�B"�B�B#�B�B
=B�B �B�B�B�B�B�BB��B �B�B"�B!�B!�B�B�B�B"�B�B�B�B�B�B�B�B!�B�BuB�B�B�B�B�B"�B�B�BuB�B�B�B�BbB�B+B)�B"�B �B#�B�B/B8RB8RB6FB:^B9XB49B8RB5?B0!B/B9XB8RB8RB>wBA�BD�BA�B:^B:^BK�BK�BK�BF�BB�BJ�BXB^5B^5B^5B^5BcTBe`BdZBaHB]/BZBYB`BB\)B^5BiyBffBdZBjBw�Bw�Bv�By�B�B�JB�\B�\B�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�3B�?B�9B�?B�?B�LB�LB�LB�LB�^B�qB�^B�wB��B��B��B��B�B�B�B�B�)B�BB�5B�B�NB�B��B��B��B��B��B��B��B��B��B	B	B��B	B	+B	oB	�B	�B	�B	�B	"�B	!�B	 �B	%�B	+B	0!B	33B	2-B	8RB	A�B	C�B	C�B	C�B	C�B	C�B	E�B	J�B	O�B	S�B	T�B	VB	W
B	YB	ZB	ZB	ZB	[#B	]/B	aHB	cTB	e`B	gmB	gmB	gmB	gmB	ffB	hsB	jB	jB	iyB	n�B	q�B	r�B	s�B	t�B	s�B	t�B	s�B	r�B	v�B	v�B	{�B	{�B	|�B	�B	�B	�B	�B	�7B	�DB	�\B	�bB	�hB	�hB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�-B	�'B	�B	�B	�'B	�FB	�LB	�?B	�LB	�jB	�}B	�}B	�wB	�wB	B	ĜB	B	ŢB	ŢB	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�B	�#B	�#B	�B	�B	�B	�B	�)B	�TB	�fB	�mB	�fB	�sB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B	��B
B
B
B
%B
B
B
B
B
%B
+B
+B
%B
%B
	7B

=B

=B
	7B
+B
	7B

=B

=B
	7B
PB
VB
uB
{B
hB
bB
hB
uB
oB
oB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
#�B
"�B
 �B
 �B
#�B
#�B
#�B
#�B
"�B
!�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
&�B
(�B
(�B
+B
+B
'�B
(�B
+B
+B
)�B
'�B
,B
.B
-B
,B
.B
-B
/B
2-B
33B
5?B
6FB
6FB
5?B
5?B
49B
33B
2-B
33B
9XB
9XB
8RB
8RB
8RB
7LB
9XB
9XB
7LB
:^B
<jB
<jB
<jB
<jB
;dB
:^B
:^B
8RB
8RB
:^B
<jB
<jB
<jB
<jB
<jB
;dB
:^B
:^B
:^B
8RB
9XB
;dB
<jB
:^B
7LB
9XB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
;dB
<jB
=qB
;dB
?}B
?}B
>wB
?}B
?}B
@�B
?}B
?}B
@�B
A�B
A�B
B�B
@�B
C�B
C�B
C�B
B�B
F�B
G�B
I�B
I�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
N�B
L�B
N�B
P�B
O�B
O�B
P�B
Q�B
P�B
P�B
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
S�B
T�B
VB
VB
VB
VB
T�B
S�B
S�B
W
B
VB
XB
YB
YB
YB
XB
W
B
YB
ZB
ZB
ZB
ZB
ZB
YB
[#B
[#B
[#B
ZB
\)B
\)B
ZB
\)B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
_;B
^5B
^5B
]/B
]/B
\)B
^5B
`BB
_;B
^5B
]/B
`BB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
e`B
gmB
hsB
iyB
hsB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
k�B
jB
l�B
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
m�B
m�B
k�B
jB
l�B
m�B
m�B
n�B
m�B
m�B
l�B
l�B
n�B
p�B
r�B
q�B
p�B
q�B
q�B
p�B
p�B
p�B
r�B
s�B
t�B
t�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BF�BH�BI�BI�BJ�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BM�BN�BN�BO�BR�BS�BW
B\)BcnBsB�B�]B��B
�B6�Bd@B��B�B�B�Br|BSuBc�B�MB��Bz�B�"B�_Bv�Bz�Bt�B�{B�B��B�B~�BvBl�Bc:BDBA�BLdBXBK^BLdBEBI�BI�BKxB>�B2�B.�B�B�B�B�B�B�B�	B�|B�nB� B��B�B�B�1BÖB�;B�pB��B��B|�Bl�Be�BkkBbhBNBG�BGzB;�B.�BOBB
��B 4B
�RB
�B
�YB
�)B
�+B
�7B
��B
�]B
��B
jB
��B
�B
v`B
n�B
poB
f�B
L~B
LB
J�B
3�B
!B
# B
/B
_B	�'B	��B	�B	�B	�aB	ÖB	�NB	�=B	�sB	�nB	��B	��B	�B	��B	��B	��B	�B	�B	�$B	�rB	y�B	u�B	}�B	m�B	v+B	j�B	Y�B	S�B	U�B	R�B	Q�B	H�B	G�B	B�B	@OB	?cB	C�B	=B	4�B	+B	#�B	
XB	�B�`B��B�B�YBՁB�{B�B��B�pBٚB҉B� B�B��B�B�B�hB�B�B��B��B��B��B��B��B��B��B��B��B�RB�nB��B��B��B{dB�:B�FB�&B�B�)B�OBwfB�B{Bj�Bm�B�AB�UB}qBu�Bu�BwfBz^B}"B{0BwBtBg�BV�Bf�B_!BS[BMjBLJBI7BRoBVmBFYB?.BB'BB�BO\BW?BTFBPHBJrBD�B2B&�B2GB%�B+�B(sB9�B-�B*KB+6B �B,B/�B.�B-�B'B+�B-�B(�B"�B�B&B!�B)_B(sB$�B �B�B#nB�B$&B�B�B5B!bB \BdB)B9BB�B��B �B BB#:B"4B"4B;BkBQB# B 'BdBOB5B 'BOBVB"4BQB�BsBVB�ByB�B#TB \BjB�B�BB�BxB:B �B+kB*eB#�B!�B$�B �B/�B8�B8�B6�B:�B9�B4�B8�B5�B1'B0;B9�B9$B9	B>�BBBEBB'B;JB;�BK�BL0BL0BG_BC�BK�BXyB^jB^jB^�B^�BcnBezBdtBa|B]�BZ�BY�B`�B]B_Bi�BgBe`BkQBxBxRBw�Bz�B��B�~B��B��B��B��B��B��B��B��B��B��B�B�B��B�'B� B�@B�@B�FB��B�[B�hB�tB��B�tB��B��B��B��B��B��B��B��B�B�)B�^B�,B�MB�EB�QB�KBؓB�xB�vB޸B��B��B��B��B��B��B��B�B�B�B�LB�.B	-B	;B�}B	�B	�B	�B	�B	�B	B	�B	"�B	!�B	!-B	&2B	+QB	0UB	3hB	2�B	8�B	A�B	C�B	C�B	C�B	C�B	C�B	FB	KB	PB	TB	UB	VB	W$B	Y1B	Z7B	Z7B	ZQB	[WB	]dB	abB	cnB	ezB	gmB	gmB	gmB	g�B	f�B	h�B	j�B	j�B	i�B	n�B	q�B	r�B	s�B	t�B	s�B	t�B	s�B	r�B	v�B	v�B	|B	|B	}VB	�'B	�3B	�GB	�gB	�RB	�^B	�\B	�bB	�NB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�B	�6B	�)B	�5B	�'B	�-B	�GB	�GB	�AB	�IB	�OB	�[B	�FB	�LB	��B	��B	��B	��B	��B	��B	��B	ªB	ĶB	��B	ŢB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�0B	�B	�B	�B	�,B	�B	�,B	�FB	�+B	�7B	�=B	�7B	�7B	�=B	�=B	�B	�QB	�_B	�mB	�xB	�nB	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�%B	�B	�B	�B	��B
�B
 B
 B
B
'B
 4B	�BB
-B
'B
B
B
MB
aB
SB
mB
?B
EB
EB
?B
YB
	RB

=B

XB
	RB
EB
	lB

rB

rB
	�B
PB
pB
uB
{B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
 �B
#�B
$�B
#�B
"�B
 �B
 �B
#�B
#�B
#�B
#�B
"�B
!�B
$�B
$�B
%�B
'B
'B
'B
(
B
(
B
(
B
'B
(�B
)B
+B
+6B
(>B
)*B
+B
+B
*B
(>B
,B
.B
-)B
,"B
./B
-CB
/5B
2-B
3MB
5%B
6`B
6FB
5?B
5%B
49B
33B
2aB
3hB
9>B
9XB
8RB
8lB
8lB
7�B
9rB
9rB
7�B
:xB
<jB
<jB
<jB
<�B
;B
:xB
:�B
8�B
8�B
:�B
<�B
<�B
<�B
<�B
<�B
;B
:�B
:�B
:�B
8�B
9�B
;B
<�B
:�B
7�B
9�B
<�B
<�B
<�B
<�B
=�B
=�B
<�B
;�B
<�B
=�B
;�B
?�B
?�B
>�B
?�B
?�B
@�B
?}B
?�B
@�B
A�B
A�B
B�B
@�B
C�B
C�B
C�B
B�B
F�B
G�B
I�B
I�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
M�B
M�B
OB
MB
N�B
Q B
O�B
O�B
Q B
Q�B
QB
Q B
Q�B
TB
T�B
T�B
UB
U2B
T�B
TB
TB
UB
TB
U2B
VB
VB
VB
VB
UB
T,B
T,B
W$B
V9B
XB
YB
YB
Y1B
X+B
W?B
Y1B
ZB
ZB
Z7B
Z7B
Z7B
YB
[=B
[=B
[=B
ZQB
\)B
\CB
Z7B
\]B
^5B
^5B
^OB
_;B
_;B
_;B
^OB
_;B
^5B
^OB
]IB
]IB
\]B
^OB
`BB
_VB
^jB
]dB
`\B
bNB
bNB
bhB
bhB
bhB
a|B
bhB
bhB
cnB
cnB
cTB
cnB
cnB
cTB
b�B
bhB
dtB
dtB
dZB
ezB
e�B
ezB
d�B
dtB
d�B
e�B
g�B
hsB
iyB
h�B
i�B
h�B
h�B
h�B
i�B
i�B
i�B
k�B
k�B
kkB
k�B
l�B
k�B
l�B
l�B
l�B
l�B
k�B
j�B
l�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n}B
n�B
m�B
m�B
k�B
j�B
l�B
m�B
m�B
n�B
m�B
m�B
l�B
l�B
n�B
p�B
r�B
q�B
p�B
q�B
q�B
p�B
p�B
p�B
r�B
s�B
t�B
t�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810030037212018100300372120181003003721201810030200152018100302001520181003020015201810040026512018100400265120181004002651  JA  ARFMdecpA19c                                                                20180929093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180929003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180929003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180929003535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180929003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180929003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180929003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180929003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180929003536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180929003536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180929005528                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180929153504  CV  JULD            G�O�G�O�F�'�                JM  ARCAJMQC2.0                                                                 20181002153721  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181002153721  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181002170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181003152651  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                