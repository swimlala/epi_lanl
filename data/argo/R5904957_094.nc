CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:21Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140821  20181024140821  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ^A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��-t1   @��%O��@3�E�����c�     1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ^A   A   A   @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffBffB   B(  B0  B7��B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cy�fC|  C~  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  D   D y�D  D� D  D� D  D� DfD� D  Dy�D  D� D  D�fDfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.fD.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D>��D?� D@  D@� DA  DA� DB  DB�fDCfDC� DD  DD�fDE  DE� DFfDF�fDGfDG� DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DN��DO� DP  DP�fDQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv� Dw  Dw� DwٚDy��D�8 D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�(�@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�BQ�BQ�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�BqQ�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B��B�u�B�u�B���C :�C:�C:�C:�C:�C
:�C:�C!GC:�C:�C:�C:�C:�C:�C:�C:�C T{C"T{C$:�C&:�C(:�C*:�C,:�C.T{C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP!GCR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�CvT{Cx:�Cz!GC|:�C~:�C�qC�qC�qC��C��C�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�*>C�*>C�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC��C�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC��C�qC�*>C�qC�qC�qC�qD �D �RD�D��D�D��D�D��DD��D�D�RD�D��D�D�DD��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!RD!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*�RD+�D+��D,�D,��D-�D-��D.D.��D/�D/�RD0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�D>�D>��D?RD?��D@�D@��DA�DA��DB�DB�DCDC��DD�DD�DE�DE��DFDF�DGDG��DH�DH��DIRDI��DJ�DJ��DK�DK��DL�DL��DM�DM�DN�DN��DORDO��DP�DP�DQ�DQ�RDR�DR��DS�DS��DT�DT��DU�DU��DV�DV�DW�DW��DX�DX��DY�DY��DZ�DZ��D[RD[��D\�D\��D]�D]��D^�D^�RD_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��DjDj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DuRDu�RDv�Dv��Dw�Dw��Dw�RDy�RD�?\D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�bA�oA�oA��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A�bA��AڶFAڅA��A�Q�A��/A���A�r�AѲ-A��A�ĜA͝�A��A�r�A�S�A�O�A�r�Aź^A�|�A�x�A�33A�bNA�  A��#A�x�A�E�A��hA���A�"�A��mA���A�jA�33A��!A��uA���A���A�~�A���A��A�C�A�bNA�l�A���A���A�r�A�\)A��A��PA��A���A��A�C�A��HA�/A��7A��mA��9A��A��RA�5?A���A�G�A�(�A�bNA�1A�C�A��mA���A��A�A�K�A���A���A{�Az{AyVAw�;Ar��AjM�Aj-Ah��Afv�Ad�Aa�7A`�RA_�;A]K�A[��AY��AWx�AV�AVbNAV�AU"�ATVAS��AQ��AQAQAP�\AO�-AO��AO/AN�ALv�AKp�AJ�uAH��AGt�AF��AF��AF�AE��ADVAC
=AA�hAA
=A@1'A?��A?A>=qA=�;A=O�A<��A<$�A:ZA8$�A7t�A6VA4�A4bNA3A3C�A2Q�A1�A0z�A.�/A.  A,M�A(�jA&�A%K�A$�A#�#A#x�A#
=A"�+A!S�A $�A��A�-A|�A��A��A�!A��A�A��A �A;dA|�Ar�A7LA�mA��AVA
��A
(�A��A�uA��AbA  AG�A{@�t�@��h@�G�@��@�bN@�
=@��\@�@��#@�?}@���@��@��@�&�@�z�@��@��
@�!@�?}@�D@�+@���@���@�|�@�\)@�5?@�A�@�@�C�@⟾@�+@�n�@�M�@�@�@���@�p�@�z�@�M�@�1@�v�@�`B@Չ7@�t�@���@̋D@�9X@���@�o@��T@�X@�%@��/@�Ĝ@�j@���@ă@���@�^5@�ȴ@�|�@��@�{@�O�@���@��h@���@��R@��w@��P@���@�v�@�n�@�V@���@���@�x�@���@���@��^@���@���@��-@�%@���@��R@�M�@��@���@�7L@��9@�z�@�Q�@�A�@�A�@�1'@�1@��F@���@���@���@�&�@���@��@�(�@���@�;d@�
=@��@�~�@��@�/@��D@�(�@��;@��P@�"�@���@��@��7@�G�@�V@��@�Ĝ@��j@�Ĝ@��j@��9@��D@��D@�z�@�9X@���@��@���@�dZ@�33@��y@���@�ff@�@���@��-@��7@�X@�O�@�?}@���@�bN@�1'@��@���@���@��P@�l�@�\)@�"�@���@���@��!@���@��\@�E�@���@��7@�`B@�G�@�V@��/@�Ĝ@��9@��D@�r�@�bN@�Q�@�9X@�(�@��@��y@��!@��\@�ff@��@���@�`B@�G�@��@���@�z�@�9X@�ƨ@�l�@�\)@�S�@�C�@�33@��@�n�@�n�@��!@���@��R@��R@��\@�{@�`B@��`@�A�@���@���@��P@��@�|�@�|�@��@��@�dZ@�C�@�;d@�;d@�o@���@���@���@���@�`B@�7L@��@�%@���@��u@�I�@�1@���@�+@��H@��@���@���@�=q@��-@�p�@�O�@�%@�Ĝ@���@�Z@�1'@�ƨ@�S�@�@�v�@�{@��T@��-@��7@�x�@�?}@��u@��m@���@���@�+@��@���@���@�hs@�V@�Ĝ@�Z@�I�@���@��w@�K�@��@�ff@�@���@�X@�/@���@�Ĝ@�Z@��;@�C�@���@�v�@�E�@�5?@�{@��#@��@�G�@�7L@��@���@��`@��9@��
@rL0@eX1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�bA�oA�oA��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A�bA��AڶFAڅA��A�Q�A��/A���A�r�AѲ-A��A�ĜA͝�A��A�r�A�S�A�O�A�r�Aź^A�|�A�x�A�33A�bNA�  A��#A�x�A�E�A��hA���A�"�A��mA���A�jA�33A��!A��uA���A���A�~�A���A��A�C�A�bNA�l�A���A���A�r�A�\)A��A��PA��A���A��A�C�A��HA�/A��7A��mA��9A��A��RA�5?A���A�G�A�(�A�bNA�1A�C�A��mA���A��A�A�K�A���A���A{�Az{AyVAw�;Ar��AjM�Aj-Ah��Afv�Ad�Aa�7A`�RA_�;A]K�A[��AY��AWx�AV�AVbNAV�AU"�ATVAS��AQ��AQAQAP�\AO�-AO��AO/AN�ALv�AKp�AJ�uAH��AGt�AF��AF��AF�AE��ADVAC
=AA�hAA
=A@1'A?��A?A>=qA=�;A=O�A<��A<$�A:ZA8$�A7t�A6VA4�A4bNA3A3C�A2Q�A1�A0z�A.�/A.  A,M�A(�jA&�A%K�A$�A#�#A#x�A#
=A"�+A!S�A $�A��A�-A|�A��A��A�!A��A�A��A �A;dA|�Ar�A7LA�mA��AVA
��A
(�A��A�uA��AbA  AG�A{@�t�@��h@�G�@��@�bN@�
=@��\@�@��#@�?}@���@��@��@�&�@�z�@��@��
@�!@�?}@�D@�+@���@���@�|�@�\)@�5?@�A�@�@�C�@⟾@�+@�n�@�M�@�@�@���@�p�@�z�@�M�@�1@�v�@�`B@Չ7@�t�@���@̋D@�9X@���@�o@��T@�X@�%@��/@�Ĝ@�j@���@ă@���@�^5@�ȴ@�|�@��@�{@�O�@���@��h@���@��R@��w@��P@���@�v�@�n�@�V@���@���@�x�@���@���@��^@���@���@��-@�%@���@��R@�M�@��@���@�7L@��9@�z�@�Q�@�A�@�A�@�1'@�1@��F@���@���@���@�&�@���@��@�(�@���@�;d@�
=@��@�~�@��@�/@��D@�(�@��;@��P@�"�@���@��@��7@�G�@�V@��@�Ĝ@��j@�Ĝ@��j@��9@��D@��D@�z�@�9X@���@��@���@�dZ@�33@��y@���@�ff@�@���@��-@��7@�X@�O�@�?}@���@�bN@�1'@��@���@���@��P@�l�@�\)@�"�@���@���@��!@���@��\@�E�@���@��7@�`B@�G�@�V@��/@�Ĝ@��9@��D@�r�@�bN@�Q�@�9X@�(�@��@��y@��!@��\@�ff@��@���@�`B@�G�@��@���@�z�@�9X@�ƨ@�l�@�\)@�S�@�C�@�33@��@�n�@�n�@��!@���@��R@��R@��\@�{@�`B@��`@�A�@���@���@��P@��@�|�@�|�@��@��@�dZ@�C�@�;d@�;d@�o@���@���@���@���@�`B@�7L@��@�%@���@��u@�I�@�1@���@�+@��H@��@���@���@�=q@��-@�p�@�O�@�%@�Ĝ@���@�Z@�1'@�ƨ@�S�@�@�v�@�{@��T@��-@��7@�x�@�?}@��u@��m@���@���@�+@��@���@���@�hs@�V@�Ĝ@�Z@�I�@���@��w@�K�@��@�ff@�@���@�X@�/@���@�Ĝ@�Z@��;@�C�@���@�v�@�E�@�5?@�{@��#@��@�G�@�7L@��@���@��`@��9@��
@rL0@eX1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBYBYBYBXBVBK�BP�BXB\)BcTBo�B}�B�-B��B�B  BB+B�B!�B#�B/BC�BB�B.B{B�yB��B��B�B��B5?BM�BP�BR�BC�B>wB7LB7LB33BJ�Bt�B��B�bB�B��B��B��B�PB33BÖB	7B:^B-B-B-BVB�B�5BŢB�^B�-B�-B��B�=BdZBI�B'�BuBB
��B
�TB
��B
e`B
C�B
"�B
"�B
O�B
P�B
-B
"�B
�B
uB	�TB	�PB	��B	��B	�hB	�DB	}�B	|�B	v�B	aHB	W
B	C�B	33B	.B	)�B	'�B	!�B	�B	�B	VB	DB	uB	�B	{B	{B	{B	{B	hB	VB	DB	B��B��B�B�B�B�yB�fB�BB�/B�B��B��B��B��BȴBĜB��B�dB�RB�RB�FB�3B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�VB�PB�DB�=B�7B�7B�1B�%B�B�JB�VB�bB�hB�uB�bB�DB�B}�B{�Bx�Bt�Bo�BjBhsBffBffBe`Be`BdZBffBgmBhsBjBn�B|�B�B�hB��B�{B�oB�bB�hB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�FB�RB�RB�RB�?B�FB�^B�dB�^B�dB��BBBĜBŢBƨBǮBǮBȴBȴBȴBɺB��B��B��B�B�sB�;B�BB�5B�;B�B��B��B	1B	\B	\B	bB	bB	bB	{B	�B	�B	�B	�B	�B	!�B	#�B	'�B	,B	1'B	49B	7LB	9XB	=qB	=qB	B�B	C�B	E�B	F�B	F�B	F�B	H�B	K�B	L�B	P�B	VB	YB	[#B	\)B	`BB	e`B	hsB	jB	l�B	o�B	t�B	{�B	�B	�1B	�=B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�TB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
1B
1B
+B
1B
1B
	7B

=B
	7B

=B
DB
PB
PB
VB
\B
bB
bB
hB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)�B
3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBYBYBYBXBVBK�BP�BXB\)BcTBo�B}�B�-B��B�B  BB+B�B!�B#�B/BC�BB�B.B{B�yB��B��B�B��B5?BM�BP�BR�BC�B>wB7LB7LB33BJ�Bt�B��B�bB�B��B��B��B�PB33BÖB	7B:^B-B-B-BVB�B�5BŢB�^B�-B�-B��B�=BdZBI�B'�BuBB
��B
�TB
��B
e`B
C�B
"�B
"�B
O�B
P�B
-B
"�B
�B
uB	�TB	�PB	��B	��B	�hB	�DB	}�B	|�B	v�B	aHB	W
B	C�B	33B	.B	)�B	'�B	!�B	�B	�B	VB	DB	uB	�B	{B	{B	{B	{B	hB	VB	DB	B��B��B�B�B�B�yB�fB�BB�/B�B��B��B��B��BȴBĜB��B�dB�RB�RB�FB�3B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�VB�PB�DB�=B�7B�7B�1B�%B�B�JB�VB�bB�hB�uB�bB�DB�B}�B{�Bx�Bt�Bo�BjBhsBffBffBe`Be`BdZBffBgmBhsBjBn�B|�B�B�hB��B�{B�oB�bB�hB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�FB�RB�RB�RB�?B�FB�^B�dB�^B�dB��BBBĜBŢBƨBǮBǮBȴBȴBȴBɺB��B��B��B�B�sB�;B�BB�5B�;B�B��B��B	1B	\B	\B	bB	bB	bB	{B	�B	�B	�B	�B	�B	!�B	#�B	'�B	,B	1'B	49B	7LB	9XB	=qB	=qB	B�B	C�B	E�B	F�B	F�B	F�B	H�B	K�B	L�B	P�B	VB	YB	[#B	\)B	`BB	e`B	hsB	jB	l�B	o�B	t�B	{�B	�B	�1B	�=B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�TB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
1B
1B
+B
1B
1B
	7B

=B
	7B

=B
DB
PB
PB
VB
\B
bB
bB
hB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)�B
3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140821                              AO  ARCAADJP                                                                    20181024140821    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140821  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140821  QCF$                G�O�G�O�G�O�0               