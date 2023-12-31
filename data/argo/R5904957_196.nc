CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:41Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140841  20181024140841  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��u�,�1   @����@5L�C���d5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�33A��A   A>ffA`  A�  A�  A�  A�  A�  A�33A�  A�  B ffB  B��B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cx  Cz  C|  C}�fC�fC��3C�  C��C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  Dy�D��Dy�D  D� D  Dy�D  Dy�D��Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D �fD!fD!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;y�D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX�fDY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]�fD^fD^� D_  D_� D`  D`� Da  Da� Da��Dby�Db��Dc� Dc��Dd� De  De� De��Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dyl)D�/
D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @333@�  @�33A��A   A>ffA`  A�  A�  A�  A�  A�  A�33A�  A�  B ffB  B��B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cx  Cz  C|  C}�fC�fC��3C�  C��C�  C�  C��3C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C��C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  Dy�D��Dy�D  D� D  Dy�D  Dy�D��Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D �fD!fD!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;y�D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDXfDX�fDY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]�fD^fD^� D_  D_� D`  D`� Da  Da� Da��Dby�Db��Dc� Dc��Dd� De  De� De��Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dyl)D�/
D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A���A�A�  A���A���A�%A�%A�%A�JA�C�A��A���A��`A�l�Aϣ�AΗ�A�dZA̛�A�\)A�  A���A�?}A�$�A��A�`BA�Q�A��-A�VA�$�A� �A�M�A��A���A���A���A�?}A���A�p�A�A�7LA���A��yA�M�A�E�A�`BA��FA�=qA��yA���A��DA��^A�C�A��hA���A�oA�  A��mA��A�E�A�A�(�A��A�7LA���A�M�A�7LA��;A�5?A��A�jA��A���A���A�bNA���A�  A�x�A�v�A��!A�;dA��A��yA�oA�A�;dA�bNA��hA���A}�
AxQ�At��AsoAmO�Aj��Ai�Af^5Aa
=A^��A]�A\E�AZ(�AY�AW��AV�HAU;dATbAR��AP��AO��AN��AL��AK;dAJȴAI��AIC�AG�^AE
=AD$�AC/AB�yAB��ABjAA�wAA33A@{A?��A?A=/A<E�A;��A;�FA;|�A:~�A9?}A8n�A81'A7�
A7
=A6�9A69XA5��A4ĜA49XA3�PA2�A1�^A0E�A.��A-�hA-�A,��A,A+33A*�+A)oA( �A%��A$bNA#|�A#7LA"�/A"A�A!G�A  �A�#A�A�DA�/Ax�A��A��AA�A�;A�/AbNA�A�wAp�A��A��A�uA��A�`Av�A�uA
�`A
�DA
1'A	�mA�A`BA+A��An�A�A�PA�yA��A�RA�+Al�@��@���@�1@���@�X@���@�Z@�S�@��@�D@��u@�Q�@�$�@@�o@�n�@�hs@�/@�V@��/@�@�j@��@��@�-@��m@�E�@���@�G�@�Q�@��@߶F@�S�@�V@ݑh@�bN@ۅ@���@��H@�V@�n�@��T@�b@֗�@�7L@�V@ԓu@ӕ�@��H@�=q@Ь@�@��@��@̛�@˅@�-@�Q�@Ǿw@�\)@���@°!@��@��@�E�@���@��@�t�@�C�@�
=@�E�@�X@��@���@��@���@��@��@�v�@��h@���@��\@�=q@��/@�"�@�C�@�S�@��@�~�@��+@��+@��\@�^5@��T@��u@��P@�;d@��y@���@���@���@��\@�M�@���@���@��@�=q@��T@���@�^5@��@�r�@��`@���@�x�@���@�j@��w@�@��@�1'@��@��@��\@�V@�V@��@�x�@��T@�M�@�ȴ@��@��@�K�@��@�~�@��#@���@�Z@���@��!@���@�X@��`@��@���@�"�@��@���@�x�@��@�A�@��@�K�@��@��H@���@��!@�^5@�E�@�@���@���@�hs@�/@�%@�%@���@���@��j@�r�@��
@��@�"�@�
=@�@��y@��!@���@�~�@�5?@��^@�x�@��`@�Z@�b@��;@��
@�\)@�n�@�-@��@��T@��h@�G�@��/@��D@��@�1'@�b@�  @��@�  @��w@��@�l�@��@��!@�n�@�V@�V@���@�@��-@���@��@�x�@�O�@��@��@�Ĝ@��j@��j@���@�j@�(�@�|�@�|�@�l�@�l�@�l�@�K�@�o@�
=@���@��@�n�@�^5@�V@�=q@��@��h@�G�@�/@���@���@�j@� �@���@�\)@��@��y@��@���@��R@���@��\@��\@��\@�~�@�~�@�v�@�=q@�{@��@��@��@�@��h@�x�@�hs@�7L@���@���@��u@��u@�(�@���@��
@��@��@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A���A�A�  A���A���A�%A�%A�%A�JA�C�A��A���A��`A�l�Aϣ�AΗ�A�dZA̛�A�\)A�  A���A�?}A�$�A��A�`BA�Q�A��-A�VA�$�A� �A�M�A��A���A���A���A�?}A���A�p�A�A�7LA���A��yA�M�A�E�A�`BA��FA�=qA��yA���A��DA��^A�C�A��hA���A�oA�  A��mA��A�E�A�A�(�A��A�7LA���A�M�A�7LA��;A�5?A��A�jA��A���A���A�bNA���A�  A�x�A�v�A��!A�;dA��A��yA�oA�A�;dA�bNA��hA���A}�
AxQ�At��AsoAmO�Aj��Ai�Af^5Aa
=A^��A]�A\E�AZ(�AY�AW��AV�HAU;dATbAR��AP��AO��AN��AL��AK;dAJȴAI��AIC�AG�^AE
=AD$�AC/AB�yAB��ABjAA�wAA33A@{A?��A?A=/A<E�A;��A;�FA;|�A:~�A9?}A8n�A81'A7�
A7
=A6�9A69XA5��A4ĜA49XA3�PA2�A1�^A0E�A.��A-�hA-�A,��A,A+33A*�+A)oA( �A%��A$bNA#|�A#7LA"�/A"A�A!G�A  �A�#A�A�DA�/Ax�A��A��AA�A�;A�/AbNA�A�wAp�A��A��A�uA��A�`Av�A�uA
�`A
�DA
1'A	�mA�A`BA+A��An�A�A�PA�yA��A�RA�+Al�@��@���@�1@���@�X@���@�Z@�S�@��@�D@��u@�Q�@�$�@@�o@�n�@�hs@�/@�V@��/@�@�j@��@��@�-@��m@�E�@���@�G�@�Q�@��@߶F@�S�@�V@ݑh@�bN@ۅ@���@��H@�V@�n�@��T@�b@֗�@�7L@�V@ԓu@ӕ�@��H@�=q@Ь@�@��@��@̛�@˅@�-@�Q�@Ǿw@�\)@���@°!@��@��@�E�@���@��@�t�@�C�@�
=@�E�@�X@��@���@��@���@��@��@�v�@��h@���@��\@�=q@��/@�"�@�C�@�S�@��@�~�@��+@��+@��\@�^5@��T@��u@��P@�;d@��y@���@���@���@��\@�M�@���@���@��@�=q@��T@���@�^5@��@�r�@��`@���@�x�@���@�j@��w@�@��@�1'@��@��@��\@�V@�V@��@�x�@��T@�M�@�ȴ@��@��@�K�@��@�~�@��#@���@�Z@���@��!@���@�X@��`@��@���@�"�@��@���@�x�@��@�A�@��@�K�@��@��H@���@��!@�^5@�E�@�@���@���@�hs@�/@�%@�%@���@���@��j@�r�@��
@��@�"�@�
=@�@��y@��!@���@�~�@�5?@��^@�x�@��`@�Z@�b@��;@��
@�\)@�n�@�-@��@��T@��h@�G�@��/@��D@��@�1'@�b@�  @��@�  @��w@��@�l�@��@��!@�n�@�V@�V@���@�@��-@���@��@�x�@�O�@��@��@�Ĝ@��j@��j@���@�j@�(�@�|�@�|�@�l�@�l�@�l�@�K�@�o@�
=@���@��@�n�@�^5@�V@�=q@��@��h@�G�@�/@���@���@�j@� �@���@�\)@��@��y@��@���@��R@���@��\@��\@��\@�~�@�~�@�v�@�=q@�{@��@��@��@�@��h@�x�@�hs@�7L@���@���@��u@��u@�(�@���@��
@��@��@k��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�
B �BS�BgmBhsBt�B�FB
=B$�B.B1'B5?B9XB>wB9XB2-B33B5?B8RB<jBA�BQ�BT�BZBgmBk�Bn�Bp�Bq�Bq�Bo�Bn�Bo�Bn�BiyBffBbNB^5B^5B\)BZBT�BI�B;dB9XB9XB7LB-B#�B!�B�B�B
=B��B�B�;BĜB�XB�-B�B��B�bBy�Bn�B\)BF�B�B
�B
�B
��B
�}B
�LB
��B
�1B
v�B
q�B
r�B
{�B
x�B
u�B
ZB
1'B
�B

=B	�B	�HB	�B	�jB	��B	��B	�hB	�DB	�B	z�B	q�B	iyB	YB	J�B	:^B	)�B	!�B	�B	bB	B	B��B��B��B�B�fB�fB�fB�`B�NB�;B�)B�B��B��B��B��B��B��B��B��B��BɺBȴBǮBŢBÖB��B�}B�qB�dB�XB�LB�FB�3B�'B�B�B�B��B��B��B��B��B��B�uB�bB�VB�DB�7B�%B�B�B�B�B|�Bz�Bw�Bs�Bo�Bl�BjBjBjBjBiyBhsBhsBgmBgmBe`BdZBcTB`BB_;B^5B[#B\)B[#B[#B[#BZBZBZB[#B[#B[#BZBZB\)B]/B^5B`BBaHBbNBbNBdZBgmBhsBhsBgmBhsBjBjBk�Bm�Bo�Bp�Bp�Bp�Bp�Bo�Bm�Bk�BgmBdZBgmBgmBhsBhsBhsBhsBl�Bo�Br�Bu�Bw�Bz�B�B�B�=B�DB�1B�1B�JB�\B�VB�VB�\B�uB��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�\B�VB�PB�PB�PB�PB�VB�VB�\B�hB��B��B��B��B�uB�hB�\B�\B�hB�\B�VB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�9BB��B�
B�;B�sB�B�B��B	1B	bB	{B	!�B	9XB	H�B	T�B	\)B	`BB	bNB	e`B	ffB	ffB	ffB	iyB	p�B	v�B	z�B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�PB	�VB	�bB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�RB	�^B	�jB	�wB	�wB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�5B	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
sB
)*111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
�
B �BS�BgmBhsBt�B�FB
=B$�B.B1'B5?B9XB>wB9XB2-B33B5?B8RB<jBA�BQ�BT�BZBgmBk�Bn�Bp�Bq�Bq�Bo�Bn�Bo�Bn�BiyBffBbNB^5B^5B\)BZBT�BI�B;dB9XB9XB7LB-B#�B!�B�B�B
=B��B�B�;BĜB�XB�-B�B��B�bBy�Bn�B\)BF�B�B
�B
�B
��B
�}B
�LB
��B
�1B
v�B
q�B
r�B
{�B
x�B
u�B
ZB
1'B
�B

=B	�B	�HB	�B	�jB	��B	��B	�hB	�DB	�B	z�B	q�B	iyB	YB	J�B	:^B	)�B	!�B	�B	bB	B	B��B��B��B�B�fB�fB�fB�`B�NB�;B�)B�B��B��B��B��B��B��B��B��B��BɺBȴBǮBŢBÖB��B�}B�qB�dB�XB�LB�FB�3B�'B�B�B�B��B��B��B��B��B��B�uB�bB�VB�DB�7B�%B�B�B�B�B|�Bz�Bw�Bs�Bo�Bl�BjBjBjBjBiyBhsBhsBgmBgmBe`BdZBcTB`BB_;B^5B[#B\)B[#B[#B[#BZBZBZB[#B[#B[#BZBZB\)B]/B^5B`BBaHBbNBbNBdZBgmBhsBhsBgmBhsBjBjBk�Bm�Bo�Bp�Bp�Bp�Bp�Bo�Bm�Bk�BgmBdZBgmBgmBhsBhsBhsBhsBl�Bo�Br�Bu�Bw�Bz�B�B�B�=B�DB�1B�1B�JB�\B�VB�VB�\B�uB��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�\B�VB�PB�PB�PB�PB�VB�VB�\B�hB��B��B��B��B�uB�hB�\B�\B�hB�\B�VB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�9BB��B�
B�;B�sB�B�B��B	1B	bB	{B	!�B	9XB	H�B	T�B	\)B	`BB	bNB	e`B	ffB	ffB	ffB	iyB	p�B	v�B	z�B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�PB	�VB	�bB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�RB	�^B	�jB	�wB	�wB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�5B	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
sB
)*111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140841                              AO  ARCAADJP                                                                    20181024140841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140841  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140841  QCF$                G�O�G�O�G�O�0               