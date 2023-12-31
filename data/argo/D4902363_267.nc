CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-06T00:35:46Z creation;2018-08-06T00:36:29Z conversion to V3.1;2019-12-19T07:35:48Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180806003546  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_267                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�wu�c�1   @�wv�`�@9��E���d\_ح��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�A�33B33B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�A�33B33B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�ZA�Q�A�E�A�C�A�I�A�E�A�?}A�?}A�=qA�7LA�33A�$�A�VAˡ�A�+A���A���A��mA�7LA�{A�JA��A�-A�;dA�K�A�+A��TA�C�A�;dA��A�1'A���A���A��A��wA�ZA��A���A�A��7A�ĜA�x�A�&�A��
A��^A���A�ĜA��7A�`BA�33A�1A�v�A���A��DA�9XA���A�C�A���A���A��A�t�A��TA��#A��A�ZA�1A��FA�7LA�bA���A���A�(�A��A�C�A��-A��A�A�z�A}A{�FAz�Az-AyhsAx�!Aw��Av�AuXAt��AtbNAs�TAsx�Ar$�Ao�An��Am��Ak��AjI�Ah��Af��Ad��Ac��Ab��A`ffA^ȴA]�A\9XA[�7A[O�A[&�AZbNAY�;AYx�AYAW`BAVZAU��AU%ATM�ASAS/AQAN�yANVAN1AM�PAM7LALȴALZAK�wAJ�yAI��AI+AH��AH�uAG��AG+AFz�AE�AD�AC�FAC�7AC33AA�
AA?}A@��A>�!A;��A;&�A:�+A:5?A9�A9�A8�yA6�A4�yA3�A3�^A3dZA2��A2bA1G�A0��A0��A0�DA0�A/+A.ȴA.1'A-?}A,�A,VA+��A+"�A)�#A)x�A(��A(JA'�TA'�A&ĜA&-A%�7A%dZA%O�A%/A%�A%VA$��A$-A"�`A"VA"1'A"bA!�wA bNAK�AȴAjA5?A�Al�A�A=qA��AhsAv�A��AA��A�A�TAA��A-A��AAVAv�A1'A��A��A
�!A	��A	G�A	%A�`A��Ar�A=qA�A�uA�;AAI�A`BA��A bN@�C�@��-@��H@�Ĝ@�K�@��R@�&�@��@�Z@��@�@�dZ@�hs@�t�@���@�O�@�I�@�;d@�^@��@��/@�\)@��@߅@�v�@�%@ۅ@�Z@ׅ@���@ՙ�@�Z@��;@�dZ@�
=@ҏ\@�$�@���@�A�@��@�Ĝ@�Q�@�l�@�M�@��T@�X@ȃ@���@�ff@ź^@�bN@�1'@��@��;@�t�@°!@���@��`@�Q�@��
@�t�@�;d@�+@�33@�"�@���@���@�`B@�V@���@�o@���@�I�@��
@�ff@��@���@��@�j@��@�+@�
=@��@���@�~�@�`B@��/@��u@�|�@�-@���@�@���@� �@���@�^5@�?}@�j@�"�@�G�@�9X@��!@��T@�hs@�X@�G�@��@���@��`@��`@��9@�A�@�  @��
@�E�@�j@�1@�ƨ@��w@�K�@�ff@���@��@��@�r�@� �@��m@�ƨ@�dZ@�@��R@�V@��-@���@�z�@�1@�|�@��H@�n�@�{@��@��^@���@�G�@��@�I�@�1@��m@�ƨ@��@�l�@�dZ@�\)@�C�@��@��!@�v�@���@��j@��@�S�@�33@�+@�"�@��@��@��R@�n�@�@���@���@��@���@�j@��P@�t�@�t�@�t�@�t�@�dZ@��@��R@�5?@��^@���@��@�A�@��@��;@���@��P@�t�@�l�@�\)@��@���@�^5@�@���@��#@���@�p�@�O�@��@��/@��u@~v�@}�@|�j@|�@{ƨ@{C�@{o@z�H@zn�@zJ@y��@y&�@w�P@w\)@w\)@w;d@v�R@v��@vv�@u��@uV@t�j@t�@t�D@tz�@tI�@t�@s��@s�@r�@r��@r�!@r^5@rJ@q��@qhs@q�@p��@p�`@p��@pĜ@p��@p�u@pbN@p  @o��@o\)@n��@n��@n5?@n@m�@m�T@m��@m�-@m/@l�/@lZ@l(�@l1@k�
@ko@j��@jn�@j=q@jJ@i��@i�7@i��@i��@i�^@i7L@i�@hQ�@g|�@g
=@f�+@f$�@e�@e`B@eO�@eV@d�@d�/@d��@dI�@c�m@c��@c"�@b��@bM�@bJ@a��@a�^@a��@ax�@ahs@aX@a%@`Ĝ@`��@`r�@`bN@`b@_�@_�@^��@^@]@]O�@\�@\I�@\(�@\�@[��@[dZ@Zn�@Z-@Y�@Yhs@X��@XbN@X  @W�w@W�P@W;d@W�@V�@V�R@VV@V$�@U@U�@U�@T�/@T�D@Tj@TZ@T9X@Sƨ@S"�@R�H@R~�@Q�^@Q7L@PĜ@PA�@O�;@O|�@OK�@O
=@N��@N5?@N@M�h@L�@L��@Lj@L9X@K�m@KC�@Ko@J��@J^5@I�#@I�7@Ihs@IG�@I7L@I�@H�9@HQ�@Hb@G��@G�@G�P@GK�@F�y@Fff@F@F@E��@E�-@E��@E�h@EO�@D�@D�@C��@Cƨ@C�F@CdZ@C"�@Co@C@B��@B-@A�#@A��@A�7@AX@A%@@��@@Ĝ@@��@@�u@@bN@@b@?��@?�P@?+@>�y@>��@>ff@>5?@=��@=�@=p�@=O�@<��@<��@<9X@<�@;ƨ@;C�@:�H@:�!@:��@:��@:�\@:^5@9��@97L@8��@8�@7��@7�@6��@65?@6$�@6{@5��@5�@5?}@4�j@4j@4(�@3�
@3dZ@333@3o@2�!@2=q@2J@2J@1��@1��@1hs@1�@0Ĝ@0�9@0r�@/�@/��@/�@/\)@/
=@.��@.ȴ@.ff@-�T@-��@-?}@,�@,z�@,j@,Z@,9X@,�@+ƨ@+dZ@+C�@+33@+@*��@*~�@*~�@*M�@*�@)�@)��@)��@)��@)hs@)�@(�`@(Ĝ@(r�@(Q�@(A�@(1'@(b@'�P@'+@&��@&��@&V@&@%�-@%�@%O�@%O�@%?}@$��@$�@$(�@#��@#�@#dZ@#@"��@"~�@"J@!��@!X@!7L@ Ĝ@ �u@ r�@ A�@ b@   @�@l�@K�@��@v�@V@E�@5?@$�@��@p�@/@V@��@�/@�j@��@j@9X@��@ƨ@��@t�@dZ@S�@C�@33@o@�@��@�!@�!@��@��@~�@n�@=q@�@��@�@��@��@�7@G�@�@�`@�u@Q�@b@�@�P@�P@|�@l�@l�@\)@K�@�@ȴ@��@V@@�@�-@�@O�@�@�/@�j@�j@�@��@�D@�D@Z@(�@1@ƨ@�F@��@dZ@C�@33@�H@�!@��@�\@�\@~�@M�@��@�#@��@hs@7L@%@�`@��@��@�9@�@1'@b@��@�w@�w@��@l�@��@v�@V@�T@@@�-@��@�h@�h@p�@`B@?}@�@j@I�@�@��@�m@ƨ@��@dZ@o@
�H@
~�@
=q@
�@
J@	��@	G�@	7L@	&�@��@�9@�@Q�@b@��@�@�@�@�P@K�@��@ȴ@�R@��@�+@$�@�@�T@�-@�@V@��@��@�D@�D@�D@z�@I�@I�@I�@I�@�@�
@ƨ@��@��@��@�@33@o@�H@��@�!@�\@n�@^5@=q@-@�@�#@�^@��@x�@G�@%@ �`@ Ĝ@ �9@ ��@ r�@ A�?��;?�\)?��?��R?���?�5??�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�ZA�Q�A�E�A�C�A�I�A�E�A�?}A�?}A�=qA�7LA�33A�$�A�VAˡ�A�+A���A���A��mA�7LA�{A�JA��A�-A�;dA�K�A�+A��TA�C�A�;dA��A�1'A���A���A��A��wA�ZA��A���A�A��7A�ĜA�x�A�&�A��
A��^A���A�ĜA��7A�`BA�33A�1A�v�A���A��DA�9XA���A�C�A���A���A��A�t�A��TA��#A��A�ZA�1A��FA�7LA�bA���A���A�(�A��A�C�A��-A��A�A�z�A}A{�FAz�Az-AyhsAx�!Aw��Av�AuXAt��AtbNAs�TAsx�Ar$�Ao�An��Am��Ak��AjI�Ah��Af��Ad��Ac��Ab��A`ffA^ȴA]�A\9XA[�7A[O�A[&�AZbNAY�;AYx�AYAW`BAVZAU��AU%ATM�ASAS/AQAN�yANVAN1AM�PAM7LALȴALZAK�wAJ�yAI��AI+AH��AH�uAG��AG+AFz�AE�AD�AC�FAC�7AC33AA�
AA?}A@��A>�!A;��A;&�A:�+A:5?A9�A9�A8�yA6�A4�yA3�A3�^A3dZA2��A2bA1G�A0��A0��A0�DA0�A/+A.ȴA.1'A-?}A,�A,VA+��A+"�A)�#A)x�A(��A(JA'�TA'�A&ĜA&-A%�7A%dZA%O�A%/A%�A%VA$��A$-A"�`A"VA"1'A"bA!�wA bNAK�AȴAjA5?A�Al�A�A=qA��AhsAv�A��AA��A�A�TAA��A-A��AAVAv�A1'A��A��A
�!A	��A	G�A	%A�`A��Ar�A=qA�A�uA�;AAI�A`BA��A bN@�C�@��-@��H@�Ĝ@�K�@��R@�&�@��@�Z@��@�@�dZ@�hs@�t�@���@�O�@�I�@�;d@�^@��@��/@�\)@��@߅@�v�@�%@ۅ@�Z@ׅ@���@ՙ�@�Z@��;@�dZ@�
=@ҏ\@�$�@���@�A�@��@�Ĝ@�Q�@�l�@�M�@��T@�X@ȃ@���@�ff@ź^@�bN@�1'@��@��;@�t�@°!@���@��`@�Q�@��
@�t�@�;d@�+@�33@�"�@���@���@�`B@�V@���@�o@���@�I�@��
@�ff@��@���@��@�j@��@�+@�
=@��@���@�~�@�`B@��/@��u@�|�@�-@���@�@���@� �@���@�^5@�?}@�j@�"�@�G�@�9X@��!@��T@�hs@�X@�G�@��@���@��`@��`@��9@�A�@�  @��
@�E�@�j@�1@�ƨ@��w@�K�@�ff@���@��@��@�r�@� �@��m@�ƨ@�dZ@�@��R@�V@��-@���@�z�@�1@�|�@��H@�n�@�{@��@��^@���@�G�@��@�I�@�1@��m@�ƨ@��@�l�@�dZ@�\)@�C�@��@��!@�v�@���@��j@��@�S�@�33@�+@�"�@��@��@��R@�n�@�@���@���@��@���@�j@��P@�t�@�t�@�t�@�t�@�dZ@��@��R@�5?@��^@���@��@�A�@��@��;@���@��P@�t�@�l�@�\)@��@���@�^5@�@���@��#@���@�p�@�O�@��@��/@��u@~v�@}�@|�j@|�@{ƨ@{C�@{o@z�H@zn�@zJ@y��@y&�@w�P@w\)@w\)@w;d@v�R@v��@vv�@u��@uV@t�j@t�@t�D@tz�@tI�@t�@s��@s�@r�@r��@r�!@r^5@rJ@q��@qhs@q�@p��@p�`@p��@pĜ@p��@p�u@pbN@p  @o��@o\)@n��@n��@n5?@n@m�@m�T@m��@m�-@m/@l�/@lZ@l(�@l1@k�
@ko@j��@jn�@j=q@jJ@i��@i�7@i��@i��@i�^@i7L@i�@hQ�@g|�@g
=@f�+@f$�@e�@e`B@eO�@eV@d�@d�/@d��@dI�@c�m@c��@c"�@b��@bM�@bJ@a��@a�^@a��@ax�@ahs@aX@a%@`Ĝ@`��@`r�@`bN@`b@_�@_�@^��@^@]@]O�@\�@\I�@\(�@\�@[��@[dZ@Zn�@Z-@Y�@Yhs@X��@XbN@X  @W�w@W�P@W;d@W�@V�@V�R@VV@V$�@U@U�@U�@T�/@T�D@Tj@TZ@T9X@Sƨ@S"�@R�H@R~�@Q�^@Q7L@PĜ@PA�@O�;@O|�@OK�@O
=@N��@N5?@N@M�h@L�@L��@Lj@L9X@K�m@KC�@Ko@J��@J^5@I�#@I�7@Ihs@IG�@I7L@I�@H�9@HQ�@Hb@G��@G�@G�P@GK�@F�y@Fff@F@F@E��@E�-@E��@E�h@EO�@D�@D�@C��@Cƨ@C�F@CdZ@C"�@Co@C@B��@B-@A�#@A��@A�7@AX@A%@@��@@Ĝ@@��@@�u@@bN@@b@?��@?�P@?+@>�y@>��@>ff@>5?@=��@=�@=p�@=O�@<��@<��@<9X@<�@;ƨ@;C�@:�H@:�!@:��@:��@:�\@:^5@9��@97L@8��@8�@7��@7�@6��@65?@6$�@6{@5��@5�@5?}@4�j@4j@4(�@3�
@3dZ@333@3o@2�!@2=q@2J@2J@1��@1��@1hs@1�@0Ĝ@0�9@0r�@/�@/��@/�@/\)@/
=@.��@.ȴ@.ff@-�T@-��@-?}@,�@,z�@,j@,Z@,9X@,�@+ƨ@+dZ@+C�@+33@+@*��@*~�@*~�@*M�@*�@)�@)��@)��@)��@)hs@)�@(�`@(Ĝ@(r�@(Q�@(A�@(1'@(b@'�P@'+@&��@&��@&V@&@%�-@%�@%O�@%O�@%?}@$��@$�@$(�@#��@#�@#dZ@#@"��@"~�@"J@!��@!X@!7L@ Ĝ@ �u@ r�@ A�@ b@   @�@l�@K�@��@v�@V@E�@5?@$�@��@p�@/@V@��@�/@�j@��@j@9X@��@ƨ@��@t�@dZ@S�@C�@33@o@�@��@�!@�!@��@��@~�@n�@=q@�@��@�@��@��@�7@G�@�@�`@�u@Q�@b@�@�P@�P@|�@l�@l�@\)@K�@�@ȴ@��@V@@�@�-@�@O�@�@�/@�j@�j@�@��@�D@�D@Z@(�@1@ƨ@�F@��@dZ@C�@33@�H@�!@��@�\@�\@~�@M�@��@�#@��@hs@7L@%@�`@��@��@�9@�@1'@b@��@�w@�w@��@l�@��@v�@V@�T@@@�-@��@�h@�h@p�@`B@?}@�@j@I�@�@��@�m@ƨ@��@dZ@o@
�H@
~�@
=q@
�@
J@	��@	G�@	7L@	&�@��@�9@�@Q�@b@��@�@�@�@�P@K�@��@ȴ@�R@��@�+@$�@�@�T@�-@�@V@��@��@�D@�D@�D@z�@I�@I�@I�@I�@�@�
@ƨ@��@��@��@�@33@o@�H@��@�!@�\@n�@^5@=q@-@�@�#@�^@��@x�@G�@%@ �`@ Ĝ@ �9@ ��@ r�@ A�?��;?�\)?��?��R?���?�5??�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BL�BL�BM�BM�BN�BM�BN�BN�BM�BN�BM�BM�BM�BN�B7LB��B�B\)B��B��B��B��B�{B�\B�=B~�B�DB� Bk�BR�BYBN�BL�BP�BVBR�BQ�BP�BL�BF�B8RB49B)�BPB�B�yB�HB�sB�`B�5B�BÖB�B�7B}�Bv�B{�B|�Bx�Bx�Br�BdZBP�B=qBF�B:^B�BPB{BJBJB  B
�B
�RB
��B
z�B
� B
r�B
]/B
L�B
Q�B
P�B
H�B
B�B
:^B
-B
/B
.B
(�B
$�B
�B
\B	��B	�B	�B	�
B	��B	ƨB	�FB	�-B	�FB	��B	��B	�bB	��B	�bB	�bB	�oB	�bB	�1B	�B	�B	x�B	jB	hsB	iyB	ffB	aHB	]/B	S�B	B�B	:^B	G�B	J�B	F�B	C�B	A�B	=qB	7LB	1'B	&�B	.B	.B	)�B	!�B	�B	�B	�B	hB		7B	oB	PB	B	B��B�B�#B�`B�fB�fB�NB�B�BÖB�^B�qBɺBŢB�}B�}B�jB�qB�}B�qB�RB�!B�3B�B��B��B�B��B��B��B��B��B��B��B��B�bB�hB�bB��B��B��B�{B�uB�\B�+B{�B�%B�7B�+B�Bt�Bu�B{�B~�B~�B|�Bx�Bu�Br�Bq�Bn�BbNBZBZBVBH�BS�BQ�BW
BQ�BK�B=qBE�BF�BF�B=qB9XB8RB6FBB�BB�BC�BA�B>wB<jB6FB.B0!B)�B+B"�B�B�B#�B �B�B�B"�B(�B#�B'�B&�B�B�B�B�B�B�B#�B �B �B�B"�B#�B�BhB�B�B�B�BhB#�B%�B!�B!�B,B)�B+B)�B'�B%�B�B�B!�B%�B$�B"�B'�B&�B#�B �B(�B(�B$�B/B0!B.B,B(�B&�B+B.B2-B5?B8RB<jB<jB<jB:^B8RB=qB=qB:^B9XB=qB33BC�B@�BE�BQ�BQ�BN�BK�BO�BR�BR�BR�BP�BM�BT�BW
BT�BT�B_;B^5BZB`BB`BB[#B\)BaHB]/B_;BffBhsBs�Bx�B}�B}�B|�B}�B}�B}�B{�By�B{�Bx�Br�Bu�B�DB�PB�\B�JB�DB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�XB�qB�qB�wB�jB�qBBƨBȴBɺBɺB��B��B��B��BɺB��B��BɺB��B��B�ZB�fB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B��B	  B	  B	  B��B��B��B	  B	B	+B	VB	�B	�B	�B	�B	$�B	%�B	'�B	'�B	&�B	,B	/B	1'B	7LB	:^B	<jB	A�B	B�B	B�B	C�B	D�B	@�B	F�B	O�B	Q�B	T�B	VB	ZB	[#B	[#B	^5B	aHB	cTB	dZB	n�B	o�B	o�B	o�B	r�B	s�B	s�B	v�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�7B	�=B	�=B	�DB	�JB	�PB	�bB	�oB	�uB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�?B	�?B	�XB	�XB	�^B	�qB	�wB	�}B	��B	��B	ĜB	ĜB	ŢB	ƨB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�
B	�
B	�B	�B	�/B	�/B	�5B	�;B	�NB	�NB	�HB	�NB	�HB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
%B
%B
B
%B
1B

=B

=B

=B

=B
	7B

=B
DB
JB
PB
PB
JB
DB
PB
VB
hB
bB
hB
hB
hB
\B
\B
bB
\B
uB
{B
uB
uB
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
"�B
#�B
#�B
"�B
 �B
�B
!�B
"�B
"�B
!�B
"�B
%�B
'�B
)�B
(�B
(�B
(�B
(�B
'�B
)�B
+B
+B
+B
-B
-B
,B
-B
/B
0!B
0!B
/B
.B
0!B
0!B
1'B
1'B
0!B
2-B
2-B
2-B
2-B
33B
33B
2-B
2-B
49B
49B
49B
6FB
7LB
7LB
7LB
7LB
6FB
6FB
8RB
9XB
8RB
8RB
9XB
:^B
9XB
:^B
:^B
;dB
:^B
;dB
:^B
9XB
;dB
<jB
;dB
<jB
=qB
=qB
<jB
;dB
<jB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
@�B
?}B
?}B
?}B
B�B
B�B
A�B
B�B
B�B
B�B
C�B
D�B
E�B
D�B
F�B
G�B
F�B
G�B
H�B
G�B
G�B
I�B
H�B
H�B
J�B
K�B
K�B
J�B
I�B
J�B
K�B
L�B
M�B
L�B
L�B
M�B
L�B
L�B
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
O�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
R�B
S�B
S�B
S�B
VB
T�B
VB
VB
VB
W
B
XB
YB
YB
XB
XB
YB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
YB
ZB
\)B
\)B
\)B
[#B
[#B
ZB
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
_;B
`BB
`BB
_;B
^5B
\)B
_;B
`BB
_;B
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
`BB
aHB
cTB
cTB
dZB
dZB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
ffB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
iyB
jB
jB
iyB
hsB
hsB
jB
jB
k�B
jB
iyB
k�B
k�B
k�B
jB
jB
k�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
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
s�B
s�B
s�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
w�B
w�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BL�BL�BM�BM�BN�BM�BN�BN�BM�BN�BNBNVBO\BS�BB�B��B�<Ba-B��B�B��B��B��B��B�dB�oB�B��Bn}BWsB[qBRBN�BR BV�BS�BRoBQNBMjBG_B9�B5%B+kB�B�B��B��B��B��B޸B��B�SB��B��B�By�B}B}�ByXBy$BsMBe�BS[B?�BG�B<B	B�B�BPB�B�B
��B
��B
��B
}B
�UB
t�B
`�B
O(B
R�B
Q�B
I�B
C�B
;�B
.�B
/�B
.�B
)�B
%�B
�B
4B	�lB	�TB	�B	�eB	ҽB	ȴB	��B	�nB	��B	��B	��B	�oB	�$B	��B	�4B	��B	��B	�B	��B	��B	y�B	lWB	i�B	jeB	g8B	b4B	^B	U2B	ESB	<�B	HfB	K)B	GEB	DB	B'B	>(B	88B	2GB	(sB	.�B	.}B	*�B	"�B	�B	�B	yB	�B	
�B	�B	"B	�B	B	 OB�OB�OB�2B�8B��B��B�QB��B�%B��B��B�	B�?B��B�OB�qB�B��B��B�	B�[B��B��B�6B��B�WB��B�B�B�WB��B�EB�	B�EB�hB�:B�B��B��B��B��B��B��B�fB}qB��B��B��B��BvzBwB|�B}BcB}qBy�Bv�Bs�BraBoiBc�B\B[�BW�BK^BU2BSBW�BR�BL�B?�BF�BG�BG_B?.B:�B9�B7�BB�BB�BC�BA�B>�B<�B7LB/�B1'B+QB,"B$ZB�B;B%B!�B]BB#�B)yB$�B(sB'mB B�B�B�B�B�B$tB!�B!�B�B#TB$ZB�B&B�B�B�B�B[B$tB&�B"�B"�B,WB*eB+kB*eB(sB&�B�BB"�B&fB%�B#�B(XB'mB$�B!�B)_B)�B%�B/OB0UB.cB,�B)�B'�B+�B.�B2�B5�B8�B<�B<�B<�B:�B9	B=�B=�B;0B:B>B4�BC�BA�BFtBRBR BOvBLdBP.BS@BS&BS&BQNBN�BU�BWsBU�BU�B_�B^�BZ�B`�B`�B\)B]BbB^OB`�BgRBiyBt9By$B~B~B}"B~B~B~B|6BzDB|6By>Bs�Bv�B�^B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�:B��B�tB�WB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�B�B�rB�pB��B�tB�B�B�B�B��B�B��B��B��B��B��B�!B�-B�TB��B	  B	 B	 B�B�BB�]B	 iB	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	($B	($B	'RB	,=B	/OB	1[B	7fB	:xB	<�B	A�B	B�B	B�B	C�B	EB	AoB	G+B	PB	R B	U2B	V9B	Z7B	[=B	[WB	^jB	a|B	c�B	d�B	n�B	o�B	o�B	o�B	r�B	s�B	tB	w2B	}B	B	�B	� B	� B	�'B	�-B	�GB	�SB	�RB	�rB	��B	�^B	�JB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�6B	�/B	�5B	�5B	�AB	�MB	�?B	�?B	�?B	�tB	�rB	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	��B	��B	żB	��B	��B	��B	��B	�B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�$B	�B	�?B	�YB	�KB	�QB	�IB	�~B	�jB	�pB	�hB	�B	�|B	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	��B	�B	�(B	�.B
 B
 OB
 B
'B
3B
3B
GB
aB
?B
?B
SB
YB
fB

XB

XB

rB

XB
	lB

XB
^B
dB
jB
jB
dB
xB
�B
pB
NB
}B
�B
�B
�B
�B
�B
}B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
"�B
#�B
#�B
"�B
 �B
 B
!�B
#B
#B
"B
# B
&B
(
B
)�B
)B
)B
)B
)B
($B
*B
+B
+B
+6B
-)B
-)B
,=B
-)B
/5B
0!B
0;B
/5B
.IB
0;B
0;B
1AB
1AB
0;B
2GB
2GB
2GB
2GB
33B
3MB
2aB
2aB
4TB
4nB
4nB
6`B
7LB
7fB
7LB
7fB
6`B
6�B
8lB
9rB
8lB
8lB
9rB
:xB
9rB
:xB
:xB
;dB
:xB
;dB
:xB
9�B
;B
<�B
;B
<�B
=qB
=�B
<�B
;�B
<�B
=�B
=�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
@�B
?�B
?�B
?�B
B�B
B�B
A�B
B�B
B�B
B�B
C�B
D�B
E�B
D�B
F�B
G�B
F�B
G�B
H�B
G�B
G�B
I�B
H�B
H�B
J�B
K�B
K�B
J�B
I�B
J�B
K�B
MB
M�B
L�B
L�B
M�B
L�B
L�B
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
O�B
P�B
P�B
P�B
P�B
Q B
Q B
O�B
Q B
Q B
P�B
Q B
Q B
Q B
O�B
Q B
Q B
Q B
QB
RB
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
TB
TB
SB
T,B
T,B
TB
VB
UB
VB
VB
VB
W$B
X+B
X�B
YB
XB
W�B
YB
X+B
X+B
X+B
X+B
X�B
Y1B
Y1B
Z7B
Z7B
Y1B
Z7B
\)B
\CB
\)B
[#B
[WB
Z7B
\CB
\CB
\CB
]dB
^OB
^OB
^OB
^5B
^OB
]IB
^OB
_VB
_;B
`BB
`BB
_VB
^OB
\xB
_VB
`\B
_VB
abB
bNB
bNB
b4B
bNB
bNB
bNB
bNB
aHB
`\B
a|B
cTB
cnB
d�B
dZB
c�B
cnB
cnB
cnB
dtB
dtB
dtB
f�B
f�B
e�B
ezB
gmB
g�B
gmB
gmB
g�B
h�B
g�B
h�B
i�B
j�B
jB
i�B
h�B
hsB
j�B
jB
k�B
j�B
i�B
k�B
k�B
k�B
j�B
j�B
k�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
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
s�B
s�B
s�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
w�B
w�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:�<�6z<Y�><#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808100034552018081000345520180810003455201808100200162018081002001620180810020016201808110025112018081100251120180811002511  JA  ARFMdecpA19c                                                                20180806093524  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180806003546  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180806003552  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180806003553  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180806003600  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180806003600  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180806003600  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180806003600  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180806003629  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180806003629                      G�O�G�O�G�O�                JA  ARUP                                                                        20180806010041                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180806153546  CV  JULD            G�O�G�O�Fû�                JM  ARCAJMQC2.0                                                                 20180809153455  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180809153455  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180809170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180810152511  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                