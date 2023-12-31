CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:07Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190607  20181005190607  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)ʶ�H1   @��*Y Q�@29�"��`�c�n��P1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�  A   A!��A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B��B��B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C�C�C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C��C��C�  C��3C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D
��Dy�D��D� D  D�fD  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D��Dy�D��D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D   D � D ��D!y�D"  D"� D#fD#� D#��D$y�D%  D%� D&fD&� D'  D'�fD(  D(� D(��D)� D)��D*y�D+  D+� D+��D,y�D-  D-� D.  D.y�D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DGy�DG��DHy�DI  DI� DJ  DJ� DK  DK� DLfDL�fDM  DMy�DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  Dey�Df  Df� Dg  Dg�fDhfDh�fDi  Di� Dj  Dj� DkfDk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq�fDrfDr� Ds  Ds�fDtfDt�fDufDu� Dv  Dv� Dw  Dw� DwٚDy��D�IHD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A!��A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B��B��B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C�C�C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C��C��C�  C��3C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D
��Dy�D��D� D  D�fD  D�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D��Dy�D��D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D   D � D ��D!y�D"  D"� D#fD#� D#��D$y�D%  D%� D&fD&� D'  D'�fD(  D(� D(��D)� D)��D*y�D+  D+� D+��D,y�D-  D-� D.  D.y�D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DGy�DG��DHy�DI  DI� DJ  DJ� DK  DK� DLfDL�fDM  DMy�DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  Dey�Df  Df� Dg  Dg�fDhfDh�fDi  Di� Dj  Dj� DkfDk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq�fDrfDr� Ds  Ds�fDtfDt�fDufDu� Dv  Dv� Dw  Dw� DwٚDy��D�IHD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A�"�A�"�A�"�A�&�A�(�A�(�A�+A�&�A�&�A�+A�(�A�+A�+A�1'A�=qA�E�A�A�A�A�A�9XA�/A�/A�-A�(�A��A�  AȺ^AȮAȡ�AȃA�A�&�Aƥ�A��A��yA�XA�Q�A�E�A�G�A��A�z�A�C�A���A��A�1'A�A�A�l�A�O�A��A���A��A�7LA��A��9A�O�A�"�A��9A��hA��A�n�A��A�%A��jA�ĜA�r�A��
A�`BA� �A��`A�\)A�/A��A�VA���A�%A��`A���A�+A�^5A�"�A���A�;dA�A�A��+A�x�A�XA�C�A�VA��A�9XA��A�`BA� �A��DA��uA��+A���A��DA��A�n�A���A��A���A�E�A��^A��A{�hArA�Am��Ai
=Ac��A^�+A\ �AYp�AU��AP�AM��AJ~�AH��AG�^AF�AEAC�;ACl�AA�TAAA>Q�A<M�A:1'A8�!A8�\A8r�A8A�A8-A7�PA5XA3�;A1��A0jA0$�A/|�A.�jA-\)A+K�A)?}A&�uA$��A%
=A%A#7LA!`BA�Az�AoA��AG�Av�A�A�A/A1'A��A�A�Ar�A�A�wAp�AdZAp�Ax�AC�AA��A5?A`BA��A�A
�`A
ȴA
n�A
A	�;A	XA�AQ�A��A	
=A�uA1'A�#A33A�AAZAVA�A7LA7LAA �yA ��A r�A $�@���@��F@���@��h@�7L@���@��j@�%@�hs@�X@��@��@�j@�j@�I�@�b@�
=@���@�M�@���@�`B@�O�@�%@�Q�@�@�/@��7@��@�^@��@@�M�@�~�@�ƨ@�n�@��y@���@�R@��@�~�@�=q@�{@��^@�?}@�p�@�hs@�V@�x�@�7@�@�hs@��@��@���@�C�@��@�@�@���@�x�@�O�@�G�@ݡ�@�@�v�@�n�@�n�@�M�@��@ݲ-@�x�@�G�@���@�r�@�j@�r�@�r�@ܣ�@�V@�t�@�v�@��@�@ى7@�G�@�V@���@؋D@ְ!@�$�@���@��/@�  @�\)@���@Ұ!@ҟ�@�v�@�5?@�p�@�I�@�l�@θR@�E�@�5?@�$�@ͩ�@��`@�  @˥�@�-@��@�&�@��@���@�bN@ǥ�@���@Ƈ+@�~�@�ff@�V@�5?@�J@���@�-@�$�@Ł@�x�@�hs@�X@�X@�X@�O�@���@��`@Ĭ@�bN@�Z@�Q�@�1'@Õ�@�"�@��H@¸R@�5?@�@�x�@�G�@��/@�z�@�Z@�9X@��@��
@��w@���@�|�@�C�@���@��\@��+@�E�@���@�V@�r�@���@�dZ@�+@���@���@�~�@�E�@���@��@�/@��@��`@��D@�bN@�I�@��@�@��H@���@��+@�^5@�J@���@�/@��`@��@��F@�dZ@���@���@��7@�?}@���@��@�G�@�/@��/@��@��@�
=@��\@��+@��+@�~�@���@���@�M�@�$�@���@���@�x�@�&�@�Ĝ@�1@��;@��@��\@�ff@���@��7@�/@���@��@��m@��@�l�@��y@�V@�J@��-@��@�G�@�/@���@��@��;@�t�@�dZ@�dZ@�\)@�33@���@���@�ff@�X@�%@���@���@��u@�z�@�9X@��w@�K�@���@��@���@��D@�Q�@���@��;@��P@�33@��@��!@�^5@�J@��^@���@�x�@�hs@�G�@�%@�Ĝ@�1'@�dZ@���@���@�@�@�@��-@��b@��@{{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A�"�A�"�A�"�A�&�A�(�A�(�A�+A�&�A�&�A�+A�(�A�+A�+A�1'A�=qA�E�A�A�A�A�A�9XA�/A�/A�-A�(�A��A�  AȺ^AȮAȡ�AȃA�A�&�Aƥ�A��A��yA�XA�Q�A�E�A�G�A��A�z�A�C�A���A��A�1'A�A�A�l�A�O�A��A���A��A�7LA��A��9A�O�A�"�A��9A��hA��A�n�A��A�%A��jA�ĜA�r�A��
A�`BA� �A��`A�\)A�/A��A�VA���A�%A��`A���A�+A�^5A�"�A���A�;dA�A�A��+A�x�A�XA�C�A�VA��A�9XA��A�`BA� �A��DA��uA��+A���A��DA��A�n�A���A��A���A�E�A��^A��A{�hArA�Am��Ai
=Ac��A^�+A\ �AYp�AU��AP�AM��AJ~�AH��AG�^AF�AEAC�;ACl�AA�TAAA>Q�A<M�A:1'A8�!A8�\A8r�A8A�A8-A7�PA5XA3�;A1��A0jA0$�A/|�A.�jA-\)A+K�A)?}A&�uA$��A%
=A%A#7LA!`BA�Az�AoA��AG�Av�A�A�A/A1'A��A�A�Ar�A�A�wAp�AdZAp�Ax�AC�AA��A5?A`BA��A�A
�`A
ȴA
n�A
A	�;A	XA�AQ�A��A	
=A�uA1'A�#A33A�AAZAVA�A7LA7LAA �yA ��A r�A $�@���@��F@���@��h@�7L@���@��j@�%@�hs@�X@��@��@�j@�j@�I�@�b@�
=@���@�M�@���@�`B@�O�@�%@�Q�@�@�/@��7@��@�^@��@@�M�@�~�@�ƨ@�n�@��y@���@�R@��@�~�@�=q@�{@��^@�?}@�p�@�hs@�V@�x�@�7@�@�hs@��@��@���@�C�@��@�@�@���@�x�@�O�@�G�@ݡ�@�@�v�@�n�@�n�@�M�@��@ݲ-@�x�@�G�@���@�r�@�j@�r�@�r�@ܣ�@�V@�t�@�v�@��@�@ى7@�G�@�V@���@؋D@ְ!@�$�@���@��/@�  @�\)@���@Ұ!@ҟ�@�v�@�5?@�p�@�I�@�l�@θR@�E�@�5?@�$�@ͩ�@��`@�  @˥�@�-@��@�&�@��@���@�bN@ǥ�@���@Ƈ+@�~�@�ff@�V@�5?@�J@���@�-@�$�@Ł@�x�@�hs@�X@�X@�X@�O�@���@��`@Ĭ@�bN@�Z@�Q�@�1'@Õ�@�"�@��H@¸R@�5?@�@�x�@�G�@��/@�z�@�Z@�9X@��@��
@��w@���@�|�@�C�@���@��\@��+@�E�@���@�V@�r�@���@�dZ@�+@���@���@�~�@�E�@���@��@�/@��@��`@��D@�bN@�I�@��@�@��H@���@��+@�^5@�J@���@�/@��`@��@��F@�dZ@���@���@��7@�?}@���@��@�G�@�/@��/@��@��@�
=@��\@��+@��+@�~�@���@���@�M�@�$�@���@���@�x�@�&�@�Ĝ@�1@��;@��@��\@�ff@���@��7@�/@���@��@��m@��@�l�@��y@�V@�J@��-@��@�G�@�/@���@��@��;@�t�@�dZ@�dZ@�\)@�33@���@���@�ff@�X@�%@���@���@��u@�z�@�9X@��w@�K�@���@��@���@��D@�Q�@���@��;@��P@�33@��@��!@�^5@�J@��^@���@�x�@�hs@�G�@�%@�Ĝ@�1'@�dZ@���@���@�@�@�@��-@��b@��@{{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	\B	VB	PB	VB	\B	\B	\B	bB	hB	hB	hB	\B	\B	\B	\B	bB	hB	�B	&�B	;dB	?}B	?}B	A�B	C�B	D�B	D�B	F�B	J�B	S�B	k�B	p�B	u�B	�B	�9B
1B
33B
S�B
gmB
��B
�B
�5B
�mB
��B+BDB
=BJB�B1'B:^BB�BL�BL�BM�BP�BT�B\)BgmBu�B�%B��B�9B��B�NB��B�B&�B=qBL�BVBjBm�Bq�Bu�Br�Bn�BiyBdZB}�B~�B�%B��B��Bt�B49B�B�RB]/Bu�BĜB�jB��B��B�\B�1B�B|�Bp�BQ�B1'B"�BJB
�/B
�B
r�B
A�B	�B	�{B	{�B	r�B	]/B	A�B	2-B	�B��B�B�mB�)B��B��B��B��B��B��BɺBǮBƨBŢBÖB��B�wB��BÖBŢBƨBƨBƨBƨBƨBɺBɺB��B��B��B��B�B�
B��B��BɺB��B��B��B��B�B�;B�HB�ZB�fB�B�B�B�B�B�B�B�B�B��B��B	B	B	%B		7B	DB	\B	DB	VB	PB	1B	B	B	JB	�B	�B	�B	�B	�B	�B	�B	+B	7LB	;dB	>wB	=qB	:^B	49B	,B	-B	1'B	0!B	,B	/B	/B	)�B	'�B	&�B	%�B	%�B	%�B	)�B	0!B	6FB	9XB	9XB	;dB	?}B	A�B	A�B	D�B	F�B	H�B	M�B	P�B	R�B	R�B	R�B	R�B	S�B	R�B	Q�B	Q�B	S�B	[#B	_;B	aHB	ZB	R�B	T�B	W
B	[#B	jB	k�B	q�B	v�B	w�B	y�B	� B	�B	� B	~�B	z�B	m�B	`BB	VB	S�B	T�B	VB	VB	W
B	YB	bNB	aHB	aHB	hsB	n�B	q�B	s�B	u�B	x�B	�B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�dB	�LB	�FB	�FB	�XB	�jB	�wB	�}B	��B	�}B	�}B	�}B	�}B	��B	��B	B	B	ÖB	ÖB	ÖB	ŢB	ǮB	ǮB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�BB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
%B
B
B
B
B
B
+B
+B
+B
+B
	7B
B
�B
$t222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	\B	VB	PB	VB	\B	\B	\B	bB	hB	hB	hB	\B	\B	\B	\B	bB	hB	�B	&�B	;dB	?}B	?}B	A�B	C�B	D�B	D�B	F�B	J�B	S�B	k�B	p�B	u�B	�B	�9B
1B
33B
S�B
gmB
��B
�B
�5B
�mB
��B+BDB
=BJB�B1'B:^BB�BL�BL�BM�BP�BT�B\)BgmBu�B�%B��B�9B��B�NB��B�B&�B=qBL�BVBjBm�Bq�Bu�Br�Bn�BiyBdZB}�B~�B�%B��B��Bt�B49B�B�RB]/Bu�BĜB�jB��B��B�\B�1B�B|�Bp�BQ�B1'B"�BJB
�/B
�B
r�B
A�B	�B	�{B	{�B	r�B	]/B	A�B	2-B	�B��B�B�mB�)B��B��B��B��B��B��BɺBǮBƨBŢBÖB��B�wB��BÖBŢBƨBƨBƨBƨBƨBɺBɺB��B��B��B��B�B�
B��B��BɺB��B��B��B��B�B�;B�HB�ZB�fB�B�B�B�B�B�B�B�B�B��B��B	B	B	%B		7B	DB	\B	DB	VB	PB	1B	B	B	JB	�B	�B	�B	�B	�B	�B	�B	+B	7LB	;dB	>wB	=qB	:^B	49B	,B	-B	1'B	0!B	,B	/B	/B	)�B	'�B	&�B	%�B	%�B	%�B	)�B	0!B	6FB	9XB	9XB	;dB	?}B	A�B	A�B	D�B	F�B	H�B	M�B	P�B	R�B	R�B	R�B	R�B	S�B	R�B	Q�B	Q�B	S�B	[#B	_;B	aHB	ZB	R�B	T�B	W
B	[#B	jB	k�B	q�B	v�B	w�B	y�B	� B	�B	� B	~�B	z�B	m�B	`BB	VB	S�B	T�B	VB	VB	W
B	YB	bNB	aHB	aHB	hsB	n�B	q�B	s�B	u�B	x�B	�B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�dB	�LB	�FB	�FB	�XB	�jB	�wB	�}B	��B	�}B	�}B	�}B	�}B	��B	��B	B	B	ÖB	ÖB	ÖB	ŢB	ǮB	ǮB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�BB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
%B
B
B
B
B
B
+B
+B
+B
+B
	7B
B
�B
$t222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190607                              AO  ARCAADJP                                                                    20181005190607    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190607  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190607  QCF$                G�O�G�O�G�O�8000            