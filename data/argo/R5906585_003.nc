CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ]   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-05-11T11:00:36Z creation      
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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  JD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  W�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  [   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  h�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  v    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  y`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    Ѐ   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Є   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Д   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         И   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        М   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    РArgo profile    3.1 1.2 19500101000000  20210511110036  20210511110036  5906585 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8354                            2B  A   NAVIS_A                         1272                            170425                          863 @�sڤ8.p1   @�s�8��@5��^5?}�eȴ9X1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�3D�C3D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�3D�C3D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aκ^AξwAδ9AθRAΰ!AθRAΧ�AΉ7AΉ7A΃A�~�A�dZA�(�A�A��/A���A;wA͸RAͶFAͶFAͲ-AͮAͬA͟�A͏\A͓uA͗�A͙�AͲ-A͸RA���A�r�A�x�Aȩ�AǴ9A�x�A�|�Aģ�A�\)A���A�v�A��A��A��TA�z�A�&�A��mA��uA�`BA�33A���A�r�A�7LA�A���A��DA�v�A�ffA�VA�(�A�%A��TA�p�A�1'A��A�oA��9A�=qA��^A�JA���A�{A�E�A��HA�=qA�\)A���A�1'A�dZA�x�A�^5A���A�JA��\A��A���A���A�K�A���A�XA�n�A�7LA�p�A�33A�bNA�ȴA���A���A���A��\A�-A�(�A�x�A�|�A�A�|�A�ZA�=qA���A��A��!A�x�A�=qA�9XA��A�|�A��+A��A���A���A���A���A�1'A���A�K�A�bA�Ax�A�A~�\A};dA{��Ay��At�uAn�\Al��Ak�AjȴAi�;Ahz�Ag��Ae%Ac33AbA�A`��A_�PA[G�AXn�AV=qAT�yATVAS�#ASx�ASVAR�RAR=qAOG�AK�#AI�PAGK�AF�HAE�AD�uAC�AB��A@�/A>=qA<��A9�7A9A8�!A7��A3��A0  A/\)A.ZA-7LA,bA)�A(1A&-A%�A#��A"�A!XA �A�HA�A�HA��A1'A1A�A��A
=A�`AĜA��A%AVA
=AȴA��A5?A�Ap�AA^5AA��A�A��A�FA��A\)A��A\)A�hA
z�A	%A�wA�A��AVAbA��A�Ax�A��A�#A;dA&�A&�A&�A+A+A�`A�^A�A �@�dZ@�;d@���@�@��-@�x�@�X@��9@��@��\@��h@��@���@�Z@�|�@�E�@�ȴ@�o@���@�V@� �@ߍP@ߝ�@��;@���@��@�~�@ۅ@�^5@�@ו�@��@���@�V@�9X@� �@�Q�@�z�@̛�@̛�@��@�V@�X@ȃ@��@�|�@�S�@�33@���@Ɨ�@Ɵ�@Ɨ�@Ƈ+@Ə\@Ə\@Ɵ�@ư!@�{@�hs@���@ě�@��;@�;d@���@�7L@��@�5?@���@�9X@���@���@�E�@��#@�hs@���@�bN@�1'@��w@�;d@�n�@���@�X@�G�@�V@�I�@��F@��@���@���@���@��m@���@�  @�1@�1@���@��y@�M�@��#@�`B@��@���@�Z@���@���@�p�@��/@��u@��
@�
=@�V@��@�@��@��@���@���@�|�@���@��@���@��T@�7L@�r�@�Q�@�9X@�9X@�9X@� �@��F@��@���@�ff@�n�@�=q@���@�hs@���@���@�A�@��m@���@��@���@��P@�S�@�K�@�dZ@�l�@�S�@��y@��@�%@���@���@�j@�9X@��P@�\)@���@�;d@�^5@���@���@��@���@�Ĝ@�1@��F@���@���@��@���@�Ĝ@��@�1'@��@���@���@�|�@��y@�^5@���@��-@�O�@�`B@�O�@�&�@��`@���@�r�@�Z@�A�@�b@��@��;@��F@�\)@���@�=q@�5?@�M�@�V@��@�p�@�?}@�V@���@�Ĝ@��@�V@���@��@��@��@��`@��9@�A�@�  @��;@��;@��@��m@��;@��;@��;@���@�ƨ@��@�\)@��@��!@��+@�E�@���@��`@���@�I�@�b@�1@��@�@�V@�J@��-@���@��@�X@���@��@��u@�I�@��
@�|�@�K�@�;d@�"�@�"�@�
=@��@��!@���@�~�@�5?@���@��@�?}@���@�I�@l�@~$�@}@}`B@|��@|z�@{�@zn�@y��@y�7@y&�@y%@x�u@x �@wl�@w
=@v�y@v��@u@v@v@u�-@tI�@s�m@s�F@s�
@s��@s@r��@r�H@r��@r�H@s@r^5@q�@q�7@qhs@qG�@p�`@pQ�@oK�@n��@nE�@m�@m/@l�@l�D@l��@lz�@l9X@l1@kƨ@kdZ@k"�@j~�@j�@i��@i�#@i��@i��@ix�@ihs@ix�@iG�@h��@hĜ@h�@g��@g�P@g|�@g�@g
=@f��@e�@e?}@eV@d��@d��@d��@eV@d��@d�@d�@d�@dj@d�@d�@c��@c��@co@b~�@a��@aX@`��@`A�@`b@_�w@^�y@^v�@^�+@^��@^ff@]��@]?}@]�@]V@\�j@\(�@[ƨ@[o@Z~�@ZJ@X�u@W�@W|�@W�@V�y@V�R@Vff@V$�@V{@V@V@U�@U�T@U�T@U�-@U�-@U�h@U�@U�@U`B@UV@T�@T�@T�@T�/@T�/@T�D@Tj@T�@S��@SS�@R�@R�H@R��@R�\@R^5@R-@Q�@Q��@Q�@Q%@PĜ@Pb@O��@OK�@N�y@N��@NE�@N@N{@N$�@M�T@M��@M�h@M?}@Lz�@K�m@K"�@J��@J-@I��@I�#@I�^@Ix�@I7L@I%@HA�@G�@F��@FE�@E��@E�h@E�@Ep�@EO�@D��@D9X@C��@CC�@B��@A�@A�^@Ahs@A%@@bN@?l�@?+@>��@>ȴ@>�+@>@=��@=p�@<��@<��@<j@;ƨ@;dZ@;@:��@:~�@:�@9��@9x�@9x�@9X@8��@8Ĝ@8�9@8Ĝ@8�9@8bN@81'@6�@6ff@6ff@6ff@6ff@6ff@6v�@6v�@6ff@6ff@6ff@6ff@6V@65?@6$�@6$�@5��@5�h@4z�@3�F@3t�@3S�@333@3"�@2��@2=q@1�#@1x�@1hs@1X@1G�@0�9@0�u@0r�@0Q�@0A�@0b@/�w@/l�@/\)@/K�@/;d@/�@.�R@.V@.E�@.5?@.{@-�@,�@,1@+ƨ@+��@+��@+�@+t�@+dZ@+C�@+"�@*�@*��@*��@*~�@*M�@*-@)��@)�^@)�7@)x�@)hs@)G�@)&�@(�9@'K�@&�y@&��@&5?@%��@%��@%V@$�@$��@$��@$j@$(�@#�m@#��@#�@"�!@!��@!��@!�7@!G�@!&�@!%@ �`@ Ĝ@ Ĝ@ Ĝ@ �9@ �9@ �u@ bN@   @�P@��@{@�-@?}@/@�@��@�/@�/@��@��@��@��@��@�j@�@��@j@j@j@Z@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aκ^AξwAδ9AθRAΰ!AθRAΧ�AΉ7AΉ7A΃A�~�A�dZA�(�A�A��/A���A;wA͸RAͶFAͶFAͲ-AͮAͬA͟�A͏\A͓uA͗�A͙�AͲ-A͸RA���A�r�A�x�Aȩ�AǴ9A�x�A�|�Aģ�A�\)A���A�v�A��A��A��TA�z�A�&�A��mA��uA�`BA�33A���A�r�A�7LA�A���A��DA�v�A�ffA�VA�(�A�%A��TA�p�A�1'A��A�oA��9A�=qA��^A�JA���A�{A�E�A��HA�=qA�\)A���A�1'A�dZA�x�A�^5A���A�JA��\A��A���A���A�K�A���A�XA�n�A�7LA�p�A�33A�bNA�ȴA���A���A���A��\A�-A�(�A�x�A�|�A�A�|�A�ZA�=qA���A��A��!A�x�A�=qA�9XA��A�|�A��+A��A���A���A���A���A�1'A���A�K�A�bA�Ax�A�A~�\A};dA{��Ay��At�uAn�\Al��Ak�AjȴAi�;Ahz�Ag��Ae%Ac33AbA�A`��A_�PA[G�AXn�AV=qAT�yATVAS�#ASx�ASVAR�RAR=qAOG�AK�#AI�PAGK�AF�HAE�AD�uAC�AB��A@�/A>=qA<��A9�7A9A8�!A7��A3��A0  A/\)A.ZA-7LA,bA)�A(1A&-A%�A#��A"�A!XA �A�HA�A�HA��A1'A1A�A��A
=A�`AĜA��A%AVA
=AȴA��A5?A�Ap�AA^5AA��A�A��A�FA��A\)A��A\)A�hA
z�A	%A�wA�A��AVAbA��A�Ax�A��A�#A;dA&�A&�A&�A+A+A�`A�^A�A �@�dZ@�;d@���@�@��-@�x�@�X@��9@��@��\@��h@��@���@�Z@�|�@�E�@�ȴ@�o@���@�V@� �@ߍP@ߝ�@��;@���@��@�~�@ۅ@�^5@�@ו�@��@���@�V@�9X@� �@�Q�@�z�@̛�@̛�@��@�V@�X@ȃ@��@�|�@�S�@�33@���@Ɨ�@Ɵ�@Ɨ�@Ƈ+@Ə\@Ə\@Ɵ�@ư!@�{@�hs@���@ě�@��;@�;d@���@�7L@��@�5?@���@�9X@���@���@�E�@��#@�hs@���@�bN@�1'@��w@�;d@�n�@���@�X@�G�@�V@�I�@��F@��@���@���@���@��m@���@�  @�1@�1@���@��y@�M�@��#@�`B@��@���@�Z@���@���@�p�@��/@��u@��
@�
=@�V@��@�@��@��@���@���@�|�@���@��@���@��T@�7L@�r�@�Q�@�9X@�9X@�9X@� �@��F@��@���@�ff@�n�@�=q@���@�hs@���@���@�A�@��m@���@��@���@��P@�S�@�K�@�dZ@�l�@�S�@��y@��@�%@���@���@�j@�9X@��P@�\)@���@�;d@�^5@���@���@��@���@�Ĝ@�1@��F@���@���@��@���@�Ĝ@��@�1'@��@���@���@�|�@��y@�^5@���@��-@�O�@�`B@�O�@�&�@��`@���@�r�@�Z@�A�@�b@��@��;@��F@�\)@���@�=q@�5?@�M�@�V@��@�p�@�?}@�V@���@�Ĝ@��@�V@���@��@��@��@��`@��9@�A�@�  @��;@��;@��@��m@��;@��;@��;@���@�ƨ@��@�\)@��@��!@��+@�E�@���@��`@���@�I�@�b@�1@��@�@�V@�J@��-@���@��@�X@���@��@��u@�I�@��
@�|�@�K�@�;d@�"�@�"�@�
=@��@��!@���@�~�@�5?@���@��@�?}@���@�I�@l�@~$�@}@}`B@|��@|z�@{�@zn�@y��@y�7@y&�@y%@x�u@x �@wl�@w
=@v�y@v��@u@v@v@u�-@tI�@s�m@s�F@s�
@s��@s@r��@r�H@r��@r�H@s@r^5@q�@q�7@qhs@qG�@p�`@pQ�@oK�@n��@nE�@m�@m/@l�@l�D@l��@lz�@l9X@l1@kƨ@kdZ@k"�@j~�@j�@i��@i�#@i��@i��@ix�@ihs@ix�@iG�@h��@hĜ@h�@g��@g�P@g|�@g�@g
=@f��@e�@e?}@eV@d��@d��@d��@eV@d��@d�@d�@d�@dj@d�@d�@c��@c��@co@b~�@a��@aX@`��@`A�@`b@_�w@^�y@^v�@^�+@^��@^ff@]��@]?}@]�@]V@\�j@\(�@[ƨ@[o@Z~�@ZJ@X�u@W�@W|�@W�@V�y@V�R@Vff@V$�@V{@V@V@U�@U�T@U�T@U�-@U�-@U�h@U�@U�@U`B@UV@T�@T�@T�@T�/@T�/@T�D@Tj@T�@S��@SS�@R�@R�H@R��@R�\@R^5@R-@Q�@Q��@Q�@Q%@PĜ@Pb@O��@OK�@N�y@N��@NE�@N@N{@N$�@M�T@M��@M�h@M?}@Lz�@K�m@K"�@J��@J-@I��@I�#@I�^@Ix�@I7L@I%@HA�@G�@F��@FE�@E��@E�h@E�@Ep�@EO�@D��@D9X@C��@CC�@B��@A�@A�^@Ahs@A%@@bN@?l�@?+@>��@>ȴ@>�+@>@=��@=p�@<��@<��@<j@;ƨ@;dZ@;@:��@:~�@:�@9��@9x�@9x�@9X@8��@8Ĝ@8�9@8Ĝ@8�9@8bN@81'@6�@6ff@6ff@6ff@6ff@6ff@6v�@6v�@6ff@6ff@6ff@6ff@6V@65?@6$�@6$�@5��@5�h@4z�@3�F@3t�@3S�@333@3"�@2��@2=q@1�#@1x�@1hs@1X@1G�@0�9@0�u@0r�@0Q�@0A�@0b@/�w@/l�@/\)@/K�@/;d@/�@.�R@.V@.E�@.5?@.{@-�@,�@,1@+ƨ@+��@+��@+�@+t�@+dZ@+C�@+"�@*�@*��@*��@*~�@*M�@*-@)��@)�^@)�7@)x�@)hs@)G�@)&�@(�9@'K�@&�y@&��@&5?@%��@%��@%V@$�@$��@$��@$j@$(�@#�m@#��@#�@"�!@!��@!��@!�7@!G�@!&�@!%@ �`@ Ĝ@ Ĝ@ Ĝ@ �9@ �9@ �u@ bN@   @�P@��@{@�-@?}@/@�@��@�/@�/@��@��@��@��@��@�j@�@��@j@j@j@Z@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
&�B
(�B
(�B
,B
0!B
8RB
C�B
H�B
K�B
L�B
Q�B
XB
[#B
bNB
m�B
��B
�BXB�=B�B�FBÖB�B�HB�B�B��B��B  B+B1B
=BJBJBDB
=BDBJBVBVBbBoBoBoB�B�B�B'�B0!B6FB<jBB�BO�B^5Br�B�JB�VB�=B�1B�Bz�Bz�Bz�By�Bw�Bu�BXBL�BN�B�B��B�qB��B��B��B��Bu�BW
BJ�BG�B<jBB�BK�BR�BVB	7B
�HB
��B
��B
}�B
v�B
s�B
p�B
m�B
hsB
bNB
^5B
N�B
G�B
9XB
0!B
'�B
 �B
0!B
-B
/B
;dB
J�B
C�B
;dB
9XB
7LB
49B
6FB
6FB
)�B
�B
	7B	�B	��B	ÖB	�}B	�XB	�?B	�B	��B	�{B	�B	{�B	q�B	iyB	]/B	O�B	F�B	@�B	<jB	8RB	7LB	5?B	33B	/B	(�B	�B	hB		7B	1B	B��B��B��B�B�B�`B�/B�B�B��BȴB�jB�XB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'BÖB�)B�;B�HB�BB�;B�`B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�TB�TB�mB�B��B��B��B��B��B	  B	B		7B	+B	+B	%B	%B	%B	B	%B	%B	B	B	B��B��B��B�B�B�B�B�B�yB�`B�`B�NB�BB�/B�B��BĜB�3B��B��B��B��B�B�B�-B�B��B��B��B��B�B{�Bx�Bt�Bs�Bu�Bw�Bx�By�B{�B|�B|�B|�B|�B}�B�B�B�=B�PB��B��B��B��B��B��B��B�'B�FB�XB�}B��B��B��B��B��B��B��B��B�
B�
B�B�B�B�5B�;B�BB�NB�TB�mB�yB�B�B�B�B��B��B��B��B��B	B		7B	DB	JB	PB	\B	�B	�B	"�B	%�B	&�B	(�B	)�B	,B	,B	,B	,B	,B	,B	,B	.B	0!B	1'B	1'B	1'B	2-B	5?B	5?B	6FB	;dB	B�B	G�B	I�B	H�B	I�B	L�B	M�B	N�B	N�B	P�B	T�B	YB	[#B	\)B	]/B	^5B	aHB	ffB	o�B	r�B	t�B	v�B	x�B	y�B	z�B	~�B	�B	�B	�B	�B	�B	�1B	�1B	�7B	�7B	�7B	�7B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	�qB	�qB	��B	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
1B
1B
	7B
	7B
	7B
DB
JB
VB
VB
VB
VB
\B
bB
bB
bB
bB
\B
hB
\B
bB
bB
oB
hB
oB
oB
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
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
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
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
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
G�B
G�B
G�B
G�B
G�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
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
T�B
T�B
T�B
T�B
T�B
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
YB
YB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
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
`BB
`BB
`BB
`BB
aHB
`BB
`BB
`BB
aH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
&�B
(�B
(�B
,B
0!B
8RB
C�B
H�B
K�B
L�B
Q�B
XB
[#B
bNB
m�B
��B
�BXB�=B�B�FBÖB�B�HB�B�B��B��B  B+B1B
=BJBJBDB
=BDBJBVBVBbBoBoBoB�B�B�B'�B0!B6FB<jBB�BO�B^5Br�B�JB�VB�=B�1B�Bz�Bz�Bz�By�Bw�Bu�BXBL�BN�B�B��B�qB��B��B��B��Bu�BW
BJ�BG�B<jBB�BK�BR�BVB	7B
�HB
��B
��B
}�B
v�B
s�B
p�B
m�B
hsB
bNB
^5B
N�B
G�B
9XB
0!B
'�B
 �B
0!B
-B
/B
;dB
J�B
C�B
;dB
9XB
7LB
49B
6FB
6FB
)�B
�B
	7B	�B	��B	ÖB	�}B	�XB	�?B	�B	��B	�{B	�B	{�B	q�B	iyB	]/B	O�B	F�B	@�B	<jB	8RB	7LB	5?B	33B	/B	(�B	�B	hB		7B	1B	B��B��B��B�B�B�`B�/B�B�B��BȴB�jB�XB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'BÖB�)B�;B�HB�BB�;B�`B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�TB�TB�mB�B��B��B��B��B��B	  B	B		7B	+B	+B	%B	%B	%B	B	%B	%B	B	B	B��B��B��B�B�B�B�B�B�yB�`B�`B�NB�BB�/B�B��BĜB�3B��B��B��B��B�B�B�-B�B��B��B��B��B�B{�Bx�Bt�Bs�Bu�Bw�Bx�By�B{�B|�B|�B|�B|�B}�B�B�B�=B�PB��B��B��B��B��B��B��B�'B�FB�XB�}B��B��B��B��B��B��B��B��B�
B�
B�B�B�B�5B�;B�BB�NB�TB�mB�yB�B�B�B�B��B��B��B��B��B	B		7B	DB	JB	PB	\B	�B	�B	"�B	%�B	&�B	(�B	)�B	,B	,B	,B	,B	,B	,B	,B	.B	0!B	1'B	1'B	1'B	2-B	5?B	5?B	6FB	;dB	B�B	G�B	I�B	H�B	I�B	L�B	M�B	N�B	N�B	P�B	T�B	YB	[#B	\)B	]/B	^5B	aHB	ffB	o�B	r�B	t�B	v�B	x�B	y�B	z�B	~�B	�B	�B	�B	�B	�B	�1B	�1B	�7B	�7B	�7B	�7B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	�qB	�qB	��B	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
1B
1B
	7B
	7B
	7B
DB
JB
VB
VB
VB
VB
\B
bB
bB
bB
bB
\B
hB
\B
bB
bB
oB
hB
oB
oB
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
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
-B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
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
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
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
G�B
G�B
G�B
G�B
G�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
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
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
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
T�B
T�B
T�B
T�B
T�B
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
YB
YB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
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
`BB
`BB
`BB
`BB
aHB
`BB
`BB
`BB
aH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210511110036                              AO  ARCAADJP                                                                    20210511110036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210511110036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210511110036  QCF$                G�O�G�O�G�O�0               