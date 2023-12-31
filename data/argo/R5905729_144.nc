CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-04-06T10:02:30Z creation      
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
resolution        =���   axis      Z        T  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     T  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ڤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ݤ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220406100230  20220406100230  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��V4 Z1   @��V���N@&��t��dBn��O�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AՉ7AՇ+AՇ+AՉ7AՉ7AՍPAՋDAՍPAՋDAՍPAՉ7AՉ7AՉ7AՉ7AՋDAՋDAՑhAՏ\AՏ\AՍPAՏ\AՑhAՓuAՓuAՓuAՕ�AՓuAՕ�A՗�A՗�Aՙ�AՕ�AՑhAՏ\AՇ+AՃA�|�A�v�A�r�A�Q�A�t�A���A�ZA�O�A�bA�hsA�JA���A��A�$�A�{A��`A���A��\A���A��A��RA�{A��jA��hA���A�|�A��A�G�AyO�Asl�AjQ�AfI�Af=qAd��Aa�#A]��A\�!A[�PAX��AVE�AUXATĜAS�^AR�jAQ"�ANn�AK�AI�AE��ABffA>��A=�A<��A<��A<r�A<^5A<E�A<A;;dA:^5A9��A9%A8(�A7�-A7`BA6�A6$�A5��A5�7A5�A4�`A4�DA4^5A3�7A2ĜA1+A0�DA/�^A.��A-�
A-��A-x�A-VA,��A,9XA, �A,(�A+�
A+;dA*�A*�A)�A)\)A);dA)�A)�A(�!A'��A'
=A&��A&~�A&M�A%�A%G�A$�A$(�A#&�A"��A"ĜA"�9A"�+A"9XA!�hA!&�A!+A ��A ��A �\A =qA��A�hA��Az�AQ�A1A��AhsA33A��AZA�A�A+A
=A�jA�A��AO�A�A��An�A�A�mA��A�A�A��AQ�A  A��A�hAS�A33A%A�yA�+AJA��A�Ar�A9XA�mAXA�A
=A��A�RAVA��A��A��A;dA�RAJAƨA��A��A �A�Ax�A"�A
n�A	�A	S�A	A��A��A�9A�A�^A�A"�A�9A�AbNA�FA7LA%A�HAȴA��A�9A��Al�A�yA��A�RA�AVAJA�;A|�A%A �jA z�A ^5A �@��@�S�@��T@�I�@��@��@���@���@���@��\@�M�@���@��@�hs@�O�@�?}@��@���@�z�@�1@��@��!@��@���@�l�@���@�~�@�5?@�J@���@�-@�h@�hs@�?}@�%@�l�@���@��@�`B@��`@�Q�@�b@��
@땁@�C�@���@�R@�h@�/@�I�@�1'@��@���@�ȴ@�ff@��@�x�@�7L@��@���@�Ĝ@�@�u@�j@�bN@�I�@��@�w@��@�$�@��@�hs@��@�Ĝ@�Q�@߮@�dZ@ޟ�@�^5@���@�%@�j@ۍP@�K�@��H@�v�@���@�G�@�b@�33@���@��y@�ȴ@֏\@�ff@��@Ցh@�?}@ԃ@Ӯ@�;d@�5?@�r�@� �@��
@�o@�ȴ@�V@�5?@�G�@���@�Z@˅@�~�@�O�@��@ȴ9@ȴ9@Ȭ@Ȭ@ȣ�@�Z@�|�@ź^@�V@��m@���@��#@��@�Ĝ@��@��D@��@�Z@��@���@��;@���@��@���@��@�b@��@�+@���@��H@��!@�n�@�$�@��@���@�p�@�V@��@�1@�S�@�o@��H@�~�@�@�A�@��@�K�@���@�{@�7L@�r�@��;@���@�dZ@�@��@�V@��@���@�Q�@� �@�|�@��H@�-@��#@�x�@�r�@��m@�t�@�"�@���@�ȴ@�5?@���@�hs@���@�Ĝ@�z�@��w@�33@�~�@�M�@��T@���@�7L@���@��m@�o@��@�G�@���@���@��@�
=@�5?@�5?@��@��T@��h@�`B@�?}@�/@��@��/@�I�@���@��P@�|�@�t�@�+@��@���@�5?@��@��^@���@�x�@�X@�/@�%@���@�z�@��w@�S�@�;d@�o@��@��+@�V@�J@��T@��7@��@���@�Ĝ@���@�r�@��
@�dZ@�K�@�;d@�"�@�
=@�@��H@��R@�=q@���@��^@���@���@���@�G�@��@�V@��/@�Ĝ@���@�Z@�(�@�  @�|�@��@��R@�M�@���@�&�@��9@�z�@�Q�@�1'@�b@���@��w@�dZ@�
=@���@�n�@�V@�=q@��@��h@�?}@�V@�%@��j@��u@��@��
@�ƨ@���@�S�@�;d@�
=@���@�v�@�ff@�ff@�^5@�E�@�{@��@���@�O�@��`@���@��D@�j@�A�@��@��@+@~V@~@}�T@}@}O�@|�D@|Z@|1@{��@z�H@z�\@zJ@y��@x��@wl�@v�R@v5?@u��@u?}@t��@tj@s�
@s"�@r�@r�H@r�H@rJ@q�^@q��@q��@q�7@qX@qG�@qX@q7L@q&�@q�@p�9@o�;@o��@o
=@n@m��@mO�@l�/@l1@k�F@kt�@kS�@kC�@ko@j�!@j~�@j=q@j�@i��@i��@i7L@h�`@hQ�@g��@gK�@g�@fȴ@fff@f5?@f$�@f{@f{@f@ep�@d�j@d�@c�m@cƨ@c��@c33@b�@b^5@bJ@a��@a��@a�@`��@`r�@`b@`  @_�@_;d@_�@^��@^{@]�h@]O�@\�j@[��@[��@[C�@Z��@ZM�@Y��@X�9@Xr�@Xr�@Xr�@X �@V��@V�@V�@Vȴ@V��@V�+@U@T��@T�/@Tz�@T�@S"�@R��@R�\@RM�@Q��@Qhs@P�`@P�u@P �@O�;@O�P@O
=@Nv�@N{@M�@M/@L��@Lz�@L(�@K�m@K33@J��@J-@JJ@I�#@I�7@IG�@I�@HĜ@H �@G��@G+@F�R@F$�@E�-@E?}@E/@E�@D�/@D1@CS�@C"�@C@B�H@B��@B�!@B�\@B�\@B�\@B~�@B~�@B~�@B^5@BM�@B-@B-@B�@A��@A��@A�@A�#@A�7@@�u@@ �@?\)@>�y@>ff@>@=�T@=��@=O�@<�@<Z@<1@;�
@;dZ@;C�@;33@;"�@;o@:��@:=q@:J@9�#@9�7@97L@9�@8Ĝ@8��@8A�@7�@7��@7��@7�w@7�P@7�@6�@6ff@6V@6@5��@5O�@5�@4��@4�@4��@4Z@41@3ƨ@3��@3C�@2�@2��@2��@2�!@2�\@1��@1��@1G�@17L@17L@17L@1%@0bN@/��@/|�@/\)@/+@/�@.��@.�R@.��@.v�@.v�@.E�@-��@,��@,z�@,Z@+��@+�
@+��@+@*�\@)��@)�^@)�7@)X@)&�@(��@(r�@(1'@(b@(  @'�@'�;@'��@'�w@'�@'\)@&v�@&E�@&E�@&5?@%�T@%�h@$�/@$1@#dZ@#o@"�H@"�\@"�@"�@"J@!��@!��@!X@!�@!�@ ��@ �@ 1'@�w@��@ff@@@?}@?}@?}@�@�/@�@Z@1@�m@ƨ@C�@@�H@=q@�@��@G�@�@r�@  @�@\)@\)@K�@+@��@�@�@�+@ff@E�@@�T@@O�@�@V@��@��@�D@Z@I�@��@t�@33@@�!@n�@�@J@�@��@hs@%@��@��@r�@1'@b@�w@K�@�@�@�@�@
=@�@ȴ@�R@�R@��@ff@E�@@�@@�-@�-@�-@��@p�@O�@��@�j@�D@�@1@�m@�m@�
@t�@dZ@C�@@
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AՉ7AՇ+AՇ+AՉ7AՉ7AՍPAՋDAՍPAՋDAՍPAՉ7AՉ7AՉ7AՉ7AՋDAՋDAՑhAՏ\AՏ\AՍPAՏ\AՑhAՓuAՓuAՓuAՕ�AՓuAՕ�A՗�A՗�Aՙ�AՕ�AՑhAՏ\AՇ+AՃA�|�A�v�A�r�A�Q�A�t�A���A�ZA�O�A�bA�hsA�JA���A��A�$�A�{A��`A���A��\A���A��A��RA�{A��jA��hA���A�|�A��A�G�AyO�Asl�AjQ�AfI�Af=qAd��Aa�#A]��A\�!A[�PAX��AVE�AUXATĜAS�^AR�jAQ"�ANn�AK�AI�AE��ABffA>��A=�A<��A<��A<r�A<^5A<E�A<A;;dA:^5A9��A9%A8(�A7�-A7`BA6�A6$�A5��A5�7A5�A4�`A4�DA4^5A3�7A2ĜA1+A0�DA/�^A.��A-�
A-��A-x�A-VA,��A,9XA, �A,(�A+�
A+;dA*�A*�A)�A)\)A);dA)�A)�A(�!A'��A'
=A&��A&~�A&M�A%�A%G�A$�A$(�A#&�A"��A"ĜA"�9A"�+A"9XA!�hA!&�A!+A ��A ��A �\A =qA��A�hA��Az�AQ�A1A��AhsA33A��AZA�A�A+A
=A�jA�A��AO�A�A��An�A�A�mA��A�A�A��AQ�A  A��A�hAS�A33A%A�yA�+AJA��A�Ar�A9XA�mAXA�A
=A��A�RAVA��A��A��A;dA�RAJAƨA��A��A �A�Ax�A"�A
n�A	�A	S�A	A��A��A�9A�A�^A�A"�A�9A�AbNA�FA7LA%A�HAȴA��A�9A��Al�A�yA��A�RA�AVAJA�;A|�A%A �jA z�A ^5A �@��@�S�@��T@�I�@��@��@���@���@���@��\@�M�@���@��@�hs@�O�@�?}@��@���@�z�@�1@��@��!@��@���@�l�@���@�~�@�5?@�J@���@�-@�h@�hs@�?}@�%@�l�@���@��@�`B@��`@�Q�@�b@��
@땁@�C�@���@�R@�h@�/@�I�@�1'@��@���@�ȴ@�ff@��@�x�@�7L@��@���@�Ĝ@�@�u@�j@�bN@�I�@��@�w@��@�$�@��@�hs@��@�Ĝ@�Q�@߮@�dZ@ޟ�@�^5@���@�%@�j@ۍP@�K�@��H@�v�@���@�G�@�b@�33@���@��y@�ȴ@֏\@�ff@��@Ցh@�?}@ԃ@Ӯ@�;d@�5?@�r�@� �@��
@�o@�ȴ@�V@�5?@�G�@���@�Z@˅@�~�@�O�@��@ȴ9@ȴ9@Ȭ@Ȭ@ȣ�@�Z@�|�@ź^@�V@��m@���@��#@��@�Ĝ@��@��D@��@�Z@��@���@��;@���@��@���@��@�b@��@�+@���@��H@��!@�n�@�$�@��@���@�p�@�V@��@�1@�S�@�o@��H@�~�@�@�A�@��@�K�@���@�{@�7L@�r�@��;@���@�dZ@�@��@�V@��@���@�Q�@� �@�|�@��H@�-@��#@�x�@�r�@��m@�t�@�"�@���@�ȴ@�5?@���@�hs@���@�Ĝ@�z�@��w@�33@�~�@�M�@��T@���@�7L@���@��m@�o@��@�G�@���@���@��@�
=@�5?@�5?@��@��T@��h@�`B@�?}@�/@��@��/@�I�@���@��P@�|�@�t�@�+@��@���@�5?@��@��^@���@�x�@�X@�/@�%@���@�z�@��w@�S�@�;d@�o@��@��+@�V@�J@��T@��7@��@���@�Ĝ@���@�r�@��
@�dZ@�K�@�;d@�"�@�
=@�@��H@��R@�=q@���@��^@���@���@���@�G�@��@�V@��/@�Ĝ@���@�Z@�(�@�  @�|�@��@��R@�M�@���@�&�@��9@�z�@�Q�@�1'@�b@���@��w@�dZ@�
=@���@�n�@�V@�=q@��@��h@�?}@�V@�%@��j@��u@��@��
@�ƨ@���@�S�@�;d@�
=@���@�v�@�ff@�ff@�^5@�E�@�{@��@���@�O�@��`@���@��D@�j@�A�@��@��@+@~V@~@}�T@}@}O�@|�D@|Z@|1@{��@z�H@z�\@zJ@y��@x��@wl�@v�R@v5?@u��@u?}@t��@tj@s�
@s"�@r�@r�H@r�H@rJ@q�^@q��@q��@q�7@qX@qG�@qX@q7L@q&�@q�@p�9@o�;@o��@o
=@n@m��@mO�@l�/@l1@k�F@kt�@kS�@kC�@ko@j�!@j~�@j=q@j�@i��@i��@i7L@h�`@hQ�@g��@gK�@g�@fȴ@fff@f5?@f$�@f{@f{@f@ep�@d�j@d�@c�m@cƨ@c��@c33@b�@b^5@bJ@a��@a��@a�@`��@`r�@`b@`  @_�@_;d@_�@^��@^{@]�h@]O�@\�j@[��@[��@[C�@Z��@ZM�@Y��@X�9@Xr�@Xr�@Xr�@X �@V��@V�@V�@Vȴ@V��@V�+@U@T��@T�/@Tz�@T�@S"�@R��@R�\@RM�@Q��@Qhs@P�`@P�u@P �@O�;@O�P@O
=@Nv�@N{@M�@M/@L��@Lz�@L(�@K�m@K33@J��@J-@JJ@I�#@I�7@IG�@I�@HĜ@H �@G��@G+@F�R@F$�@E�-@E?}@E/@E�@D�/@D1@CS�@C"�@C@B�H@B��@B�!@B�\@B�\@B�\@B~�@B~�@B~�@B^5@BM�@B-@B-@B�@A��@A��@A�@A�#@A�7@@�u@@ �@?\)@>�y@>ff@>@=�T@=��@=O�@<�@<Z@<1@;�
@;dZ@;C�@;33@;"�@;o@:��@:=q@:J@9�#@9�7@97L@9�@8Ĝ@8��@8A�@7�@7��@7��@7�w@7�P@7�@6�@6ff@6V@6@5��@5O�@5�@4��@4�@4��@4Z@41@3ƨ@3��@3C�@2�@2��@2��@2�!@2�\@1��@1��@1G�@17L@17L@17L@1%@0bN@/��@/|�@/\)@/+@/�@.��@.�R@.��@.v�@.v�@.E�@-��@,��@,z�@,Z@+��@+�
@+��@+@*�\@)��@)�^@)�7@)X@)&�@(��@(r�@(1'@(b@(  @'�@'�;@'��@'�w@'�@'\)@&v�@&E�@&E�@&5?@%�T@%�h@$�/@$1@#dZ@#o@"�H@"�\@"�@"�@"J@!��@!��@!X@!�@!�@ ��@ �@ 1'@�w@��@ff@@@?}@?}@?}@�@�/@�@Z@1@�m@ƨ@C�@@�H@=q@�@��@G�@�@r�@  @�@\)@\)@K�@+@��@�@�@�+@ff@E�@@�T@@O�@�@V@��@��@�D@Z@I�@��@t�@33@@�!@n�@�@J@�@��@hs@%@��@��@r�@1'@b@�w@K�@�@�@�@�@
=@�@ȴ@�R@�R@��@ff@E�@@�@@�-@�-@�-@��@p�@O�@��@�j@�D@�@1@�m@�m@�
@t�@dZ@C�@@
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	gmB	ffB	ffB	gmB	gmB	gmB	gmB	gmB	gmB	hsB	jB	k�B	n�B	p�B	q�B	w�B	��B	��B
��B
�B
�PB
{�B
�B
�B@�BJ�BR�B�B\B
�BB
y�B
S�B
;dB
P�B
K�B
G�B	�B
"�B	�B	��B	�B	|�B	�+B	��B	�XB	��B	��B	�B	��B	�wB	�wB	��B	�B	ǮB	�wB	�!B	��B	��B	��B	��B	��B	�-B	ȴB	�B
JB
VB
{B
uB
bB
	7B
DB
�B
�B
#�B
33B
A�B
I�B
O�B
bNB
u�B
y�B
~�B
�DB
�\B
�VB
�{B
�JB
�uB
��B
��B
��B
�B
�B
�B
�B
�B
�-B
�?B
�-B
��B
�B
��B
�B
�3B
�LB
�LB
�FB
�3B
�B
�9B
�RB
�dB
�^B
�FB
�!B
�!B
�B
�B
�-B
�FB
�9B
�'B
�B
�B
�B
�FB
�?B
�9B
�-B
�B
�B
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
��B
��B
��B
��B
��B
�hB
�oB
�uB
�{B
�bB
�bB
�bB
�bB
�VB
�VB
�=B
�=B
�DB
�DB
�JB
�JB
�=B
�JB
�=B
�1B
�B
~�B
}�B
y�B
{�B
}�B
{�B
x�B
{�B
}�B
{�B
y�B
w�B
u�B
x�B
u�B
s�B
n�B
k�B
p�B
p�B
hsB
cTB
iyB
e`B
cTB
]/B
[#B
cTB
cTB
cTB
e`B
bNB
[#B
[#B
]/B
[#B
XB
ZB
YB
P�B
S�B
XB
XB
W
B
W
B
S�B
N�B
C�B
E�B
O�B
N�B
L�B
J�B
H�B
G�B
D�B
A�B
D�B
D�B
D�B
A�B
>wB
=qB
6FB
33B
<jB
>wB
@�B
B�B
A�B
@�B
>wB
=qB
?}B
@�B
@�B
?}B
=qB
;dB
8RB
6FB
49B
0!B
-B
'�B
,B
2-B
5?B
6FB
7LB
6FB
7LB
6FB
49B
2-B
.B
$�B
)�B
.B
,B
0!B
0!B
33B
2-B
1'B
/B
.B
-B
'�B
-B
+B
1'B
0!B
+B
/B
/B
,B
2-B
2-B
33B
2-B
33B
33B
2-B
2-B
2-B
1'B
.B
+B
'�B
&�B
+B
-B
,B
)�B
(�B
&�B
(�B
%�B
'�B
%�B
"�B
%�B
"�B
(�B
'�B
&�B
$�B
"�B
�B
#�B
)�B
,B
+B
)�B
)�B
'�B
$�B
%�B
"�B
�B
 �B
�B
�B
"�B
#�B
 �B
#�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
!�B
 �B
�B
�B
VB

=B
PB
PB
hB
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
	7B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
%�B
$�B
 �B
#�B
&�B
%�B
%�B
%�B
!�B
#�B
#�B
(�B
'�B
'�B
'�B
%�B
!�B
"�B
"�B
$�B
)�B
,B
'�B
$�B
(�B
1'B
1'B
/B
/B
1'B
2-B
2-B
1'B
/B
,B
.B
1'B
33B
2-B
0!B
/B
0!B
/B
0!B
2-B
33B
33B
2-B
2-B
2-B
0!B
/B
-B
1'B
5?B
49B
33B
33B
6FB
5?B
6FB
5?B
5?B
7LB
9XB
8RB
6FB
33B
6FB
;dB
;dB
;dB
;dB
;dB
:^B
8RB
7LB
8RB
=qB
=qB
=qB
=qB
;dB
;dB
>wB
=qB
=qB
=qB
;dB
<jB
<jB
9XB
9XB
;dB
9XB
8RB
:^B
=qB
?}B
@�B
@�B
@�B
@�B
?}B
=qB
>wB
?}B
A�B
B�B
B�B
@�B
?}B
A�B
C�B
D�B
B�B
B�B
A�B
D�B
F�B
E�B
E�B
F�B
F�B
F�B
E�B
I�B
I�B
H�B
H�B
G�B
G�B
E�B
D�B
E�B
G�B
K�B
J�B
J�B
J�B
I�B
H�B
H�B
L�B
M�B
L�B
K�B
I�B
L�B
L�B
K�B
J�B
L�B
K�B
J�B
I�B
G�B
L�B
N�B
O�B
Q�B
Q�B
P�B
P�B
Q�B
S�B
T�B
S�B
Q�B
R�B
T�B
T�B
T�B
S�B
T�B
VB
T�B
T�B
S�B
Q�B
P�B
S�B
R�B
Q�B
VB
VB
VB
T�B
YB
YB
ZB
ZB
ZB
YB
ZB
ZB
ZB
ZB
YB
W
B
YB
XB
XB
[#B
[#B
[#B
[#B
]/B
^5B
^5B
]/B
\)B
ZB
YB
\)B
^5B
_;B
^5B
]/B
]/B
]/B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
^5B
^5B
_;B
_;B
_;B
_;B
aHB
aHB
_;B
`BB
^5B
^5B
bNB
dZB
cTB
aHB
^5B
dZB
e`B
dZB
cTB
bNB
`BB
`BB
e`B
dZB
cTB
aHB
e`B
e`B
e`B
dZB
e`B
e`B
ffB
ffB
gmB
ffB
ffB
ffB
gmB
iyB
gmB
hsB
jB
iyB
jB
hsB
iyB
jB
l�B
l�B
k�B
l�B
l�B
k�B
jB
jB
k�B
k�B
k�B
l�B
l�B
o�B
n�B
l�B
jB
k�B
o�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
o�B
l�B
o�B
n�B
p�B
p�B
r�B
s�B
s�B
r�B
r�B
r�B
t�B
u�B
t�B
v�B
w�B
v�B
v�B
u�B
s�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
x�B
w�B
x�B
x�B
z�B
y�B
y�B
x�B
z�B
{�B
{�B
z�B
y�B
z�B
z�B
{�B
z�B
z�B
{�B
|�B
{�B
z�B
y�B
z�B
{�B
}�B
}�B
}�B
{�B
z�B
z�B
|�B
}�B
}�B
~�B
~�B
}�B
~�B
~�B
~�B
}�B
{�B
z�B
|�B
� B
~�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�%B
�+B
�%B
�B
�B
�B
�B
�B
�+B
�+B
�1B
�+B
�7B
�7B
�7B
�7B
�1B
�7B
�=B
�7B
�7B
�7B
�1B
�+B
�1B
�=B
�DB
�=B
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
�JB
�PB
�VB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�bB
�bB
�oB
�oB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�oB
�oB
�oB
�hB
�uB
�uB
�uB
�{B
�uB
�uB
�uB
�uB
�oB
�{B
�{B
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	gmB	ffB	ffB	gmB	gmB	gmB	gmB	gmB	gmB	hsB	jB	k�B	n�B	p�B	q�B	w�B	��B	��B
��B
�B
�PB
{�B
�B
�B@�BJ�BR�B�B\B
�BB
y�B
S�B
;dB
P�B
K�B
G�B	�B
"�B	�B	��B	�B	|�B	�+B	��B	�XB	��B	��B	�B	��B	�wB	�wB	��B	�B	ǮB	�wB	�!B	��B	��B	��B	��B	��B	�-B	ȴB	�B
JB
VB
{B
uB
bB
	7B
DB
�B
�B
#�B
33B
A�B
I�B
O�B
bNB
u�B
y�B
~�B
�DB
�\B
�VB
�{B
�JB
�uB
��B
��B
��B
�B
�B
�B
�B
�B
�-B
�?B
�-B
��B
�B
��B
�B
�3B
�LB
�LB
�FB
�3B
�B
�9B
�RB
�dB
�^B
�FB
�!B
�!B
�B
�B
�-B
�FB
�9B
�'B
�B
�B
�B
�FB
�?B
�9B
�-B
�B
�B
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
��B
��B
��B
��B
��B
�hB
�oB
�uB
�{B
�bB
�bB
�bB
�bB
�VB
�VB
�=B
�=B
�DB
�DB
�JB
�JB
�=B
�JB
�=B
�1B
�B
~�B
}�B
y�B
{�B
}�B
{�B
x�B
{�B
}�B
{�B
y�B
w�B
u�B
x�B
u�B
s�B
n�B
k�B
p�B
p�B
hsB
cTB
iyB
e`B
cTB
]/B
[#B
cTB
cTB
cTB
e`B
bNB
[#B
[#B
]/B
[#B
XB
ZB
YB
P�B
S�B
XB
XB
W
B
W
B
S�B
N�B
C�B
E�B
O�B
N�B
L�B
J�B
H�B
G�B
D�B
A�B
D�B
D�B
D�B
A�B
>wB
=qB
6FB
33B
<jB
>wB
@�B
B�B
A�B
@�B
>wB
=qB
?}B
@�B
@�B
?}B
=qB
;dB
8RB
6FB
49B
0!B
-B
'�B
,B
2-B
5?B
6FB
7LB
6FB
7LB
6FB
49B
2-B
.B
$�B
)�B
.B
,B
0!B
0!B
33B
2-B
1'B
/B
.B
-B
'�B
-B
+B
1'B
0!B
+B
/B
/B
,B
2-B
2-B
33B
2-B
33B
33B
2-B
2-B
2-B
1'B
.B
+B
'�B
&�B
+B
-B
,B
)�B
(�B
&�B
(�B
%�B
'�B
%�B
"�B
%�B
"�B
(�B
'�B
&�B
$�B
"�B
�B
#�B
)�B
,B
+B
)�B
)�B
'�B
$�B
%�B
"�B
�B
 �B
�B
�B
"�B
#�B
 �B
#�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
!�B
 �B
�B
�B
VB

=B
PB
PB
hB
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
	7B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
%�B
$�B
 �B
#�B
&�B
%�B
%�B
%�B
!�B
#�B
#�B
(�B
'�B
'�B
'�B
%�B
!�B
"�B
"�B
$�B
)�B
,B
'�B
$�B
(�B
1'B
1'B
/B
/B
1'B
2-B
2-B
1'B
/B
,B
.B
1'B
33B
2-B
0!B
/B
0!B
/B
0!B
2-B
33B
33B
2-B
2-B
2-B
0!B
/B
-B
1'B
5?B
49B
33B
33B
6FB
5?B
6FB
5?B
5?B
7LB
9XB
8RB
6FB
33B
6FB
;dB
;dB
;dB
;dB
;dB
:^B
8RB
7LB
8RB
=qB
=qB
=qB
=qB
;dB
;dB
>wB
=qB
=qB
=qB
;dB
<jB
<jB
9XB
9XB
;dB
9XB
8RB
:^B
=qB
?}B
@�B
@�B
@�B
@�B
?}B
=qB
>wB
?}B
A�B
B�B
B�B
@�B
?}B
A�B
C�B
D�B
B�B
B�B
A�B
D�B
F�B
E�B
E�B
F�B
F�B
F�B
E�B
I�B
I�B
H�B
H�B
G�B
G�B
E�B
D�B
E�B
G�B
K�B
J�B
J�B
J�B
I�B
H�B
H�B
L�B
M�B
L�B
K�B
I�B
L�B
L�B
K�B
J�B
L�B
K�B
J�B
I�B
G�B
L�B
N�B
O�B
Q�B
Q�B
P�B
P�B
Q�B
S�B
T�B
S�B
Q�B
R�B
T�B
T�B
T�B
S�B
T�B
VB
T�B
T�B
S�B
Q�B
P�B
S�B
R�B
Q�B
VB
VB
VB
T�B
YB
YB
ZB
ZB
ZB
YB
ZB
ZB
ZB
ZB
YB
W
B
YB
XB
XB
[#B
[#B
[#B
[#B
]/B
^5B
^5B
]/B
\)B
ZB
YB
\)B
^5B
_;B
^5B
]/B
]/B
]/B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
^5B
^5B
_;B
_;B
_;B
_;B
aHB
aHB
_;B
`BB
^5B
^5B
bNB
dZB
cTB
aHB
^5B
dZB
e`B
dZB
cTB
bNB
`BB
`BB
e`B
dZB
cTB
aHB
e`B
e`B
e`B
dZB
e`B
e`B
ffB
ffB
gmB
ffB
ffB
ffB
gmB
iyB
gmB
hsB
jB
iyB
jB
hsB
iyB
jB
l�B
l�B
k�B
l�B
l�B
k�B
jB
jB
k�B
k�B
k�B
l�B
l�B
o�B
n�B
l�B
jB
k�B
o�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
q�B
o�B
l�B
o�B
n�B
p�B
p�B
r�B
s�B
s�B
r�B
r�B
r�B
t�B
u�B
t�B
v�B
w�B
v�B
v�B
u�B
s�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
x�B
w�B
x�B
x�B
z�B
y�B
y�B
x�B
z�B
{�B
{�B
z�B
y�B
z�B
z�B
{�B
z�B
z�B
{�B
|�B
{�B
z�B
y�B
z�B
{�B
}�B
}�B
}�B
{�B
z�B
z�B
|�B
}�B
}�B
~�B
~�B
}�B
~�B
~�B
~�B
}�B
{�B
z�B
|�B
� B
~�B
� B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�%B
�+B
�%B
�B
�B
�B
�B
�B
�+B
�+B
�1B
�+B
�7B
�7B
�7B
�7B
�1B
�7B
�=B
�7B
�7B
�7B
�1B
�+B
�1B
�=B
�DB
�=B
�PB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�PB
�JB
�PB
�VB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�bB
�bB
�oB
�oB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�oB
�oB
�oB
�hB
�uB
�uB
�uB
�{B
�uB
�uB
�uB
�uB
�oB
�{B
�{B
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220406100230                              AO  ARCAADJP                                                                    20220406100230    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220406100230  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220406100230  QCF$                G�O�G�O�G�O�4000            