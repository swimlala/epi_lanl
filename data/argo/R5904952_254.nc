CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:02Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190602  20181005190602  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���d���1   @������@1�Q���c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A>ffA`  A�  A�  A���A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C��3C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D��D� DfD� D  D� D  D� D  D� D  D�fD  D� D  Dy�D  D�fD  D� D  D�fDfD� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D   D �fD!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,�fD,��D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3y�D3��D4y�D4��D5� D6  D6� D7  D7� D8  D8y�D8��D9� D:  D:�fD;fD;� D;��D<� D=  D=y�D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DCfDC�fDD  DD� DE  DEy�DE��DFy�DG  DG� DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DL  DL�fDM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DQ��DRy�DR��DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DYfDY� DZ  DZ�fD[fD[�fD\  D\� D]  D]� D^  D^� D_  D_� D`fD`�fDafDa� Db  Db� Dc  Dc� DdfDd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� DkfDk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� DtfDt�fDu  Du� Dv  Dvy�Dw  Dw� Dw�fDy�)D�?
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�\)A�A#�ABzAc�A��
A��
A���A��
A��
A��
A��
A��
B �B�B�B�B �B(�B1Q�B9Q�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C T{C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C T{C":�C$:�C&:�C(!GC*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC��C�qC�qC�qC�qC�*>C�qC�qC�qC��C�qC�qC��C��C��C��C�qC�qC��C�qC�qC�qC�*>C�qC��C�qC�qC��C�qC�*>C�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC��C�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D	�D	��D
�D
��D�D�RD�D��DRD��DD��D�D��D�D��D�D��D�D�D�D��D�D�RD�D�D�D��D�D�DD��D�D��D�D�RD�D��D�D�RD�D��D�D��D�D��D �D �D!D!��D"�D"��D#�D#��D$�D$��D%�D%��D&RD&��D'�D'�RD(�D(��D)�D)��D*�D*��D+�D+�D,D,�D-RD-��D.�D.��D/�D/��D0�D0�D1�D1��D2�D2��D3�D3�RD4RD4�RD5RD5��D6�D6��D7�D7��D8�D8�RD9RD9��D:�D:�D;D;��D<RD<��D=�D=�RD>�D>�D?�D?��D@�D@��DA�DA��DB�DB��DCDC�DD�DD��DE�DE�RDFRDF�RDG�DG��DHRDH�RDIRDI�RDJRDJ�RDKRDK�RDL�DL�DM�DM��DN�DN��DODO��DP�DP��DQ�DQ��DRRDR�RDSRDS��DT�DT��DU�DU��DV�DV��DWDW��DX�DX��DYDY��DZ�DZ�D[D[�D\�D\��D]�D]��D^�D^��D_�D_��D`D`�DaDa��Db�Db��Dc�Dc��DdDd��DeRDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di�Dj�Dj��DkDk��Dl�Dl�RDm�Dm��Dn�Dn��Do�Do�RDp�Dp��Dq�Dq��Dr�Dr�RDs�Ds��DtDt�Du�Du��Dv�Dv�RDw�Dw��Dw�Dy��D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�I�A�G�A�E�A�A�A�{AɁA�A�=qA��
AƩ�A�l�A�K�A�oA��A���AőhA�G�A��
A�E�AÉ7A�9XA�%A�A��A��-A�ZA���A�5?A��A��RA��\A�33A�A�  A���A���A��A�\)A�=qA�JA���A��A���A��A�(�A�\)A���A��HA��A��;A�G�A�-A�/A��HA�$�A���A�{A�/A�v�A�$�A��7A���A�t�A�A���A���A���A�"�A��A��A�jA���A��A�  A���A�
=A�ƨA�(�A�ȴA��yA���A�S�A�hsA��-A��uA�/A�bNA��mA���A�I�A�n�A�l�A��\A�|�A~��A|�Ay�Av$�At �Aql�AoO�Am?}Ai�;Ac�7A`��A]+AX��AV�jAUS�AT��ATbAQS�AN�yAL��AK��AJA�AI�FAG�ACXA=t�A9"�A8(�A6�A5S�A3��A0��A/�-A/t�A/G�A/"�A/oA.ȴA-A+�FA+��A+��A+%A*ffA*=qA*JA(�A&��A%��A%33A"�+A�-Ax�AA�A�
A?}A�AjA �A��A�+A`BAXA��Av�Ap�A�9A�
AQ�A�
A�AbNA33A
ZA	|�A�A��AI�AoA�DA{A ��A r�A ^5A -@���@��@�dZ@�+@�+@�+@�+@�+@�o@��y@��@��@�hs@��@��\@���@�r�@�1@���@��m@��@�-@�x�@��;@��@@���@�C�@�1'@��/@���@�j@�A�@��@�Ĝ@��/@��`@�&�@�-@�(�@��@�Z@��`@���@���@���@���@�!@��y@�ȴ@�5?@�b@�@�j@�O�@�-@�^@���@�D@� �@�1@�@�o@�^@㝲@�~�@�hs@�ƨ@�o@ݡ�@���@ۍP@ف@�|�@�K�@�t�@�K�@�;d@�n�@�7L@�$�@�-@ڏ\@���@�1@�bN@��@ץ�@֟�@�@��@��#@�ff@��@��y@�n�@�5?@��@թ�@��@�C�@�ȴ@�M�@���@�7L@���@��@�C�@�@���@·+@�=q@�J@���@�G�@���@̬@�z�@ˍP@�ȴ@ʟ�@�~�@�=q@�O�@�O�@�G�@�Ĝ@�  @Ǖ�@�K�@ƸR@Ə\@�J@���@��T@�?}@�b@��@��y@��@¸R@°!@°!@�M�@��;@�l�@��@�33@��@�ȴ@��+@�n�@�-@��#@�x�@��`@��u@��
@��y@��\@�n�@�ff@�V@�E�@���@��7@�X@�7L@��j@�1'@��@��@���@�@���@�X@���@�  @�t�@�
=@��@�ff@�5?@�{@��^@��@�O�@���@�Q�@�
=@��+@��R@���@���@��H@��H@��@��R@��@��@�X@�7L@���@�j@�j@�z�@���@�r�@�Z@�b@��P@�K�@�@���@�E�@�@��^@�`B@�V@��@�(�@�+@�@��y@�ȴ@�5?@���@��-@�/@��@�1'@�b@��@�1@�b@�1@���@��@��+@�ff@��@���@���@��D@�j@�I�@���@��F@��P@�K�@��y@��+@�^5@�E�@�@���@�O�@�&�@���@��@�bN@�bN@�(�@��P@�@�n�@�=q@���@���@��7@�hs@��@��`@�Ĝ@��9@���@�Z@� �@�  @��@��F@�C�@���@�ff@���@�`B@��`@��@�9X@��m@�l�@�@���@�~�@�n�@�ff@�E�@���@��D@�Z@�9X@�b@�1@��@��F@�l�@�dZ@�C�@��@���@�V@�$�@�J@���@���@��b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�M�A�I�A�G�A�E�A�A�A�{AɁA�A�=qA��
AƩ�A�l�A�K�A�oA��A���AőhA�G�A��
A�E�AÉ7A�9XA�%A�A��A��-A�ZA���A�5?A��A��RA��\A�33A�A�  A���A���A��A�\)A�=qA�JA���A��A���A��A�(�A�\)A���A��HA��A��;A�G�A�-A�/A��HA�$�A���A�{A�/A�v�A�$�A��7A���A�t�A�A���A���A���A�"�A��A��A�jA���A��A�  A���A�
=A�ƨA�(�A�ȴA��yA���A�S�A�hsA��-A��uA�/A�bNA��mA���A�I�A�n�A�l�A��\A�|�A~��A|�Ay�Av$�At �Aql�AoO�Am?}Ai�;Ac�7A`��A]+AX��AV�jAUS�AT��ATbAQS�AN�yAL��AK��AJA�AI�FAG�ACXA=t�A9"�A8(�A6�A5S�A3��A0��A/�-A/t�A/G�A/"�A/oA.ȴA-A+�FA+��A+��A+%A*ffA*=qA*JA(�A&��A%��A%33A"�+A�-Ax�AA�A�
A?}A�AjA �A��A�+A`BAXA��Av�Ap�A�9A�
AQ�A�
A�AbNA33A
ZA	|�A�A��AI�AoA�DA{A ��A r�A ^5A -@���@��@�dZ@�+@�+@�+@�+@�+@�o@��y@��@��@�hs@��@��\@���@�r�@�1@���@��m@��@�-@�x�@��;@��@@���@�C�@�1'@��/@���@�j@�A�@��@�Ĝ@��/@��`@�&�@�-@�(�@��@�Z@��`@���@���@���@���@�!@��y@�ȴ@�5?@�b@�@�j@�O�@�-@�^@���@�D@� �@�1@�@�o@�^@㝲@�~�@�hs@�ƨ@�o@ݡ�@���@ۍP@ف@�|�@�K�@�t�@�K�@�;d@�n�@�7L@�$�@�-@ڏ\@���@�1@�bN@��@ץ�@֟�@�@��@��#@�ff@��@��y@�n�@�5?@��@թ�@��@�C�@�ȴ@�M�@���@�7L@���@��@�C�@�@���@·+@�=q@�J@���@�G�@���@̬@�z�@ˍP@�ȴ@ʟ�@�~�@�=q@�O�@�O�@�G�@�Ĝ@�  @Ǖ�@�K�@ƸR@Ə\@�J@���@��T@�?}@�b@��@��y@��@¸R@°!@°!@�M�@��;@�l�@��@�33@��@�ȴ@��+@�n�@�-@��#@�x�@��`@��u@��
@��y@��\@�n�@�ff@�V@�E�@���@��7@�X@�7L@��j@�1'@��@��@���@�@���@�X@���@�  @�t�@�
=@��@�ff@�5?@�{@��^@��@�O�@���@�Q�@�
=@��+@��R@���@���@��H@��H@��@��R@��@��@�X@�7L@���@�j@�j@�z�@���@�r�@�Z@�b@��P@�K�@�@���@�E�@�@��^@�`B@�V@��@�(�@�+@�@��y@�ȴ@�5?@���@��-@�/@��@�1'@�b@��@�1@�b@�1@���@��@��+@�ff@��@���@���@��D@�j@�I�@���@��F@��P@�K�@��y@��+@�^5@�E�@�@���@�O�@�&�@���@��@�bN@�bN@�(�@��P@�@�n�@�=q@���@���@��7@�hs@��@��`@�Ĝ@��9@���@�Z@� �@�  @��@��F@�C�@���@�ff@���@�`B@��`@��@�9X@��m@�l�@�@���@�~�@�n�@�ff@�E�@���@��D@�Z@�9X@�b@�1@��@��F@�l�@�dZ@�C�@��@���@�V@�$�@�J@���@���@��b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B	uB	.B	_;B	� B	w�B	s�B	q�B	�B	�FB	�;B
oB
1'B
\)B
��B
�RB
�!B
�^B
B
��B
�yB  BVB!�B/B=qBE�BK�BQ�BR�BS�BVBVBT�BR�BQ�BO�B`BBq�B��B�B�qBƨBÖBƨB�B�/BoB&�B(�B-B1'B<jB@�BC�BG�BH�BK�BT�BYB_;BffBe`B^5BO�B9XB)�B$�BB��B��B�Bq�BJ�B�B1B
�fB
�3B
�JB
R�B
2-B	��B	�)B	�B	��B	�B	�yB	��B
B
  B	�B	�
B	ɺB	�3B	��B	�bB	� B	q�B	cTB	M�B	-B	�B	
=B��B�B�B�B�B��B	B		7B	\B	{B	oB	\B	B�`B��BĜB�}B�^B�LB�FB�FB�FB�FB�FB�?B�3B��B��B��B�;B�`B�B�B�B��B	B	B	  B�B�BB��BƨB�jB��BĜBĜBȴB�HB�B��B��B	B	%B	1B	JB	VB		7B	B	B��B��B��B��B�B�B�yB�fB�fB�mB�mB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	+B	DB	JB	\B	hB	bB	VB	PB	\B	bB	bB	bB	hB	�B	�B	�B	&�B	7LB	G�B	P�B	VB	VB	W
B	ZB	\)B	]/B	^5B	aHB	x�B	{�B	{�B	~�B	�B	�%B	�+B	�%B	�1B	�=B	�PB	�VB	�\B	�JB	�%B	�B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	�oB	�=B	�+B	�%B	�B	�%B	�B	� B	|�B	v�B	t�B	~�B	�B	�B	�B	�B	�B	��B	�RB	�wB	�dB	�XB	�}B	��B	�}B	�dB	�^B	�wB	��B	ƨB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ɺB	ȴB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�5B	�/B	�/B	�/B	�/B	�)B	�)B	�B	�B	�
B	�/B	�5B	�5B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�ZB	�ZB	�TB	�ZB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
DB
JB
JB
JB
JB
VB
PB
PB
PB
VB
VB
\B
\B
VB
\B
\B
\B
\B
bB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B��B��B��B��B��B	uB	.B	_;B	� B	w�B	s�B	q�B	�B	�FB	�;B
oB
1'B
\)B
��B
�RB
�!B
�^B
B
��B
�yB  BVB!�B/B=qBE�BK�BQ�BR�BS�BVBVBT�BR�BQ�BO�B`BBq�B��B�B�qBƨBÖBƨB�B�/BoB&�B(�B-B1'B<jB@�BC�BG�BH�BK�BT�BYB_;BffBe`B^5BO�B9XB)�B$�BB��B��B�Bq�BJ�B�B1B
�fB
�3B
�JB
R�B
2-B	��B	�)B	�B	��B	�B	�yB	��B
B
  B	�B	�
B	ɺB	�3B	��B	�bB	� B	q�B	cTB	M�B	-B	�B	
=B��B�B�B�B�B��B	B		7B	\B	{B	oB	\B	B�`B��BĜB�}B�^B�LB�FB�FB�FB�FB�FB�?B�3B��B��B��B�;B�`B�B�B�B��B	B	B	  B�B�BB��BƨB�jB��BĜBĜBȴB�HB�B��B��B	B	%B	1B	JB	VB		7B	B	B��B��B��B��B�B�B�yB�fB�fB�mB�mB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	+B	DB	JB	\B	hB	bB	VB	PB	\B	bB	bB	bB	hB	�B	�B	�B	&�B	7LB	G�B	P�B	VB	VB	W
B	ZB	\)B	]/B	^5B	aHB	x�B	{�B	{�B	~�B	�B	�%B	�+B	�%B	�1B	�=B	�PB	�VB	�\B	�JB	�%B	�B	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	�oB	�=B	�+B	�%B	�B	�%B	�B	� B	|�B	v�B	t�B	~�B	�B	�B	�B	�B	�B	��B	�RB	�wB	�dB	�XB	�}B	��B	�}B	�dB	�^B	�wB	��B	ƨB	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ɺB	ȴB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�5B	�/B	�/B	�/B	�/B	�)B	�)B	�B	�B	�
B	�/B	�5B	�5B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�ZB	�ZB	�TB	�ZB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
DB
JB
JB
JB
JB
VB
PB
PB
PB
VB
VB
\B
\B
VB
\B
\B
\B
\B
bB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
'�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190602                              AO  ARCAADJP                                                                    20181005190602    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190602  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190602  QCF$                G�O�G�O�G�O�8000            