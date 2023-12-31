CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:35Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190535  20181005190535  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���y\��1   @������@0���v��c�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A��A   A>ffA`  A�  A�  A�  A�  A�  A�  A�33A�33B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B��B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�33B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C��3C�  C��C��C�  C��3C�  C��C�  C��3C��3C�  C�  C��3C�  C�  C�  C��3C��C�  C��3C�  C��3C�  C�  C��C�  C�  C��3C��3C��C�  C��3C�  D   D y�D  D�fD  Dy�D��D� D  D� D  D� D  D� D  Dy�D��Dy�D	  D	�fD
  D
y�D
��Dy�D  D� D  D� DfD� D  D� D  Dy�D��D� D  D� D  Dy�D��D� DfD�fD  D� D  D� D  D� D  D� D��D� DfD� D��D� DfD� D  D� D  D�fD fD � D!  D!�fD"  D"� D#  D#y�D#��D$� D%  D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.fD.� D/  D/� D0  D0� D1fD1�fD2  D2y�D3  D3�fD4  D4� D5  D5�fD6fD6� D7  D7�fD8fD8� D9  D9� D:  D:� D;  D;� D<  D<� DH� DI  DIy�DJ  DJ�fDK  DK� DLfDL� DM  DM�fDN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^y�D_  D_� D`  D`� D`��Day�Db  Db�fDc  Dcy�Dc��Ddy�De  De�fDffDf� Df��Dgy�Dg��Dh� Di  Di� DjfDj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dw� Dy�qD�:�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�\)AG�A#�ABzAc�A��
A��
A��
A��
A��
A��
A�
=A�
=B �B	Q�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�BqQ�Bx�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�BȨ�B̨�BШ�B�u�B�u�B�u�B�u�B�u�B��)B�B�B�B�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$T{C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`T{Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC��C��C�qC�*>C�qC�qC�qC��C�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�*>C�qC�qC�qC�qC�*>C�qC�qC�*>C�*>C�qC�qC��C�qC�*>C�*>C�qC��C�qC�*>C�qC��C��C�qC�qC��C�qC�qC�qC��C�*>C�qC��C�qC��C�qC�qC�*>C�qC�qC��C��C�*>C�qC��C�qD �D �RD�D�D�D�RDRD��D�D��D�D��D�D��D�D�RDRD�RD	�D	�D
�D
�RDRD�RD�D��D�D��DD��D�D��D�D�RDRD��D�D��D�D�RDRD��DD�D�D��D�D��D�D��D�D��DRD��DD��DRD��DD��D�D��D�D�D D ��D!�D!�D"�D"��D#�D#�RD$RD$��D%�D%��D&D&�D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,RD,��D-�D-��D.D.��D/�D/��D0�D0��D1D1�D2�D2�RD3�D3�D4�D4��D5�D5�D6D6��D7�D7�D8D8��D9�D9��D:�D:��D;�D;��D<�D<��DH��DI�DI�RDJ�DJ�DK�DK��DLDL��DM�DM�DN�DN��DO�DO�RDP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DUDU��DV�DV��DW�DW��DXRDX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^RD^�RD_�D_��D`�D`��DaRDa�RDb�Db�Dc�Dc�RDdRDd�RDe�De�DfDf��DgRDg�RDhRDh��Di�Di��DjDj��DkRDk�RDl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqDq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�RDw�Dy�)D�B=D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��mA��A���A��A���A���A���A���A�  A�  A�  A�A�%A�%A�A�  A�%A�A�A�%A�%A�1A�1A�
=A�
=A�JA�oA�{A�{A��A��A��A��A��A��A��A��A��A�$�A�(�A�&�A�(�A�+A�/A�/A�9XA�=qA�=qA�=qA�?}A�=qA�/A��A�bA���A�7LA�K�A�K�A�n�A�
=AʼjA�  A�33A���AƅAę�A���A�n�A�/A�E�A��-A���A��A��A��^A���A���A��;A�XA�n�A�{A���A�  A���A���A�=qA��`A�VA�E�A�7LA�{A�p�A�VA���A���A��A�?}A���A�t�A�O�A�v�A��mA��#A�
=A�l�A���A��-A��+A���A��+A�A�A���A��
A���A�TA~r�A|�A{��Au33Aq;dAlAjȴAh^5Agt�Af��AedZA_�AZ�AU+AP�`AP �AO��AN��AM;dAJE�AF�\AC�A@�DA>1A9oA7�7A7K�A7C�A6�A4��A3�A2�9A1+A.��A-��A,��A+�mA*�yA)l�A(��A'�#A'
=A&��A&=qA%"�A$�A#ƨA!�wA ��A7LA�Av�AM�A�-A7LA��A��A��A�A��A�A
=A5?A��A^5A��A��AdZAjAƨA
�A	
=A��A%A+A
=A�HA1'A�#A?}AƨA��A�A�mAXA Z@��P@���@��!@��@���@�Ĝ@�
=@�9X@��\@��-@���@�j@��u@�V@�I�@�@��@�@�G�@�b@�ȴ@�?}@�(�@�K�@�"�@�@ꗍ@�p�@�\@� �@��@��H@��@��u@߶F@��@�J@�&�@�;d@�E�@�-@�`B@��@�X@��@���@�9X@�"�@��@ѩ�@ёh@��@�5?@��@�V@�(�@ϝ�@�+@�ȴ@��@�1@�;d@ʸR@�^5@�p�@�p�@�?}@ȓu@�z�@�S�@ź^@�Z@���@�$�@�&�@��@���@���@�G�@�G�@��9@�K�@�^5@���@��^@���@�@�5?@���@�M�@�@��#@���@��@��F@�
=@�^5@���@��@�^5@�^5@���@�p�@�%@���@�b@�S�@��!@��7@�/@��u@�(�@��@�  @�C�@���@��y@��!@�ff@��@���@��j@�(�@���@�S�@��@���@��+@�$�@���@�`B@�V@�Z@��@�33@��!@�~�@�^5@���@�@���@�$�@��-@���@�x�@�hs@�O�@�%@���@��j@�A�@��@��@�K�@�@��H@���@���@�-@��@��7@���@���@��D@�bN@��;@�dZ@�33@��@�o@�@��H@��!@�1@��@��@�+@�o@��y@��\@�=q@���@�%@���@�z�@�r�@�I�@��F@�
=@��+@�=q@��^@�@���@�x�@��@��@���@��@��m@��P@��;@���@��@��!@��R@���@�v�@�J@�x�@��@�Z@�  @��;@�ƨ@�ƨ@�ƨ@� �@�Q�@� �@��@�  @��w@�|�@�33@���@�^5@�M�@�-@�{@��T@�7L@�V@�Ĝ@�j@��w@�S�@�33@��y@�~�@�n�@��!@��\@�M�@�$�@��T@���@��@�`B@��@��@��/@��@�Q�@�I�@��@��@��P@�K�@�@���@��R@���@�~�@�V@���@���@��@�p�@�/�@}f�@e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��mA��A���A��A���A���A���A���A�  A�  A�  A�A�%A�%A�A�  A�%A�A�A�%A�%A�1A�1A�
=A�
=A�JA�oA�{A�{A��A��A��A��A��A��A��A��A��A�$�A�(�A�&�A�(�A�+A�/A�/A�9XA�=qA�=qA�=qA�?}A�=qA�/A��A�bA���A�7LA�K�A�K�A�n�A�
=AʼjA�  A�33A���AƅAę�A���A�n�A�/A�E�A��-A���A��A��A��^A���A���A��;A�XA�n�A�{A���A�  A���A���A�=qA��`A�VA�E�A�7LA�{A�p�A�VA���A���A��A�?}A���A�t�A�O�A�v�A��mA��#A�
=A�l�A���A��-A��+A���A��+A�A�A���A��
A���A�TA~r�A|�A{��Au33Aq;dAlAjȴAh^5Agt�Af��AedZA_�AZ�AU+AP�`AP �AO��AN��AM;dAJE�AF�\AC�A@�DA>1A9oA7�7A7K�A7C�A6�A4��A3�A2�9A1+A.��A-��A,��A+�mA*�yA)l�A(��A'�#A'
=A&��A&=qA%"�A$�A#ƨA!�wA ��A7LA�Av�AM�A�-A7LA��A��A��A�A��A�A
=A5?A��A^5A��A��AdZAjAƨA
�A	
=A��A%A+A
=A�HA1'A�#A?}AƨA��A�A�mAXA Z@��P@���@��!@��@���@�Ĝ@�
=@�9X@��\@��-@���@�j@��u@�V@�I�@�@��@�@�G�@�b@�ȴ@�?}@�(�@�K�@�"�@�@ꗍ@�p�@�\@� �@��@��H@��@��u@߶F@��@�J@�&�@�;d@�E�@�-@�`B@��@�X@��@���@�9X@�"�@��@ѩ�@ёh@��@�5?@��@�V@�(�@ϝ�@�+@�ȴ@��@�1@�;d@ʸR@�^5@�p�@�p�@�?}@ȓu@�z�@�S�@ź^@�Z@���@�$�@�&�@��@���@���@�G�@�G�@��9@�K�@�^5@���@��^@���@�@�5?@���@�M�@�@��#@���@��@��F@�
=@�^5@���@��@�^5@�^5@���@�p�@�%@���@�b@�S�@��!@��7@�/@��u@�(�@��@�  @�C�@���@��y@��!@�ff@��@���@��j@�(�@���@�S�@��@���@��+@�$�@���@�`B@�V@�Z@��@�33@��!@�~�@�^5@���@�@���@�$�@��-@���@�x�@�hs@�O�@�%@���@��j@�A�@��@��@�K�@�@��H@���@���@�-@��@��7@���@���@��D@�bN@��;@�dZ@�33@��@�o@�@��H@��!@�1@��@��@�+@�o@��y@��\@�=q@���@�%@���@�z�@�r�@�I�@��F@�
=@��+@�=q@��^@�@���@�x�@��@��@���@��@��m@��P@��;@���@��@��!@��R@���@�v�@�J@�x�@��@�Z@�  @��;@�ƨ@�ƨ@�ƨ@� �@�Q�@� �@��@�  @��w@�|�@�33@���@�^5@�M�@�-@�{@��T@�7L@�V@�Ĝ@�j@��w@�S�@�33@��y@�~�@�n�@��!@��\@�M�@�$�@��T@���@��@�`B@��@��@��/@��@�Q�@�I�@��@��@��P@�K�@�@���@��R@���@�~�@�V@���@���@��@�p�@�/�@}f�@e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�jB
�jB
�dB
�jB
�qB
�dB
�qB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�qB
�wB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
��B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
ĜB
ÖB
ŢB
ŢB
ǮB
ƨB
ɺB
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
�fB�B6FB��B�?B��B�B��B��BB�BL�BhsBr�Bv�BgmBXBA�B#�B�fB�3BǮB��BB&�B7LBF�B;dB!�B{B%B�B�B��B�
B�NB�fB�)B��BÖBÖB�B~�Bn�BgmB`BBH�B.B{B%B
�HB
ƨB
�FB
��B
gmB
B�B
�B	�sB	�B	��B	�FB	��B	t�B	ZB	<jB	49B	&�B	$�B	&�B	�B	B�ZBɺB�3B�!B�B��B��B�oB�B}�B�B�B� B�B�B�B�B�VB�hB�{B��B��B�B�'B�3B�?B�XB�^B�dB�dB�dB�dB�}B�wB�wB�dB�wB�wB�dB�XB�RB�LB�?B�FB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�3B�9B�9B�LB�qBBĜBƨBĜBĜBĜBŢBŢBǮB��B��BǮBŢB��B��B��B�B�B�ZB�B�`B�fB�`B�ZB�BB�5B�/B�)B�;B�fB�B�B�B�B�B�B�B�B�sB�`B�fB�fB�ZB�;B�5B�5B�;B�BB�;B�;B�;B�ZB�mB�fB�B��B	B	%B	+B	DB	JB	JB	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	(�B	$�B	$�B	(�B	)�B	-B	9XB	@�B	J�B	K�B	L�B	J�B	K�B	N�B	N�B	P�B	R�B	XB	_;B	dZB	e`B	e`B	hsB	m�B	p�B	q�B	p�B	q�B	t�B	x�B	z�B	|�B	� B	�B	�%B	�1B	�7B	�DB	�PB	�VB	�bB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�FB	�LB	�LB	�LB	�XB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
%B
1B
1B
1B
1B
1B
1B
1B
	7B
1B
+B
%B
1B
1B
+B
1B
DB
JB
JB
JB
PB
VB
VB
VB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
{B
�B
�B
�B
#�B
.2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�jB
�jB
�dB
�jB
�qB
�dB
�qB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�}B
�}B
�wB
�qB
�wB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
��B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
ĜB
ÖB
ŢB
ŢB
ǮB
ƨB
ɺB
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
�fB�B6FB��B�?B��B�B��B��BB�BL�BhsBr�Bv�BgmBXBA�B#�B�fB�3BǮB��BB&�B7LBF�B;dB!�B{B%B�B�B��B�
B�NB�fB�)B��BÖBÖB�B~�Bn�BgmB`BBH�B.B{B%B
�HB
ƨB
�FB
��B
gmB
B�B
�B	�sB	�B	��B	�FB	��B	t�B	ZB	<jB	49B	&�B	$�B	&�B	�B	B�ZBɺB�3B�!B�B��B��B�oB�B}�B�B�B� B�B�B�B�B�VB�hB�{B��B��B�B�'B�3B�?B�XB�^B�dB�dB�dB�dB�}B�wB�wB�dB�wB�wB�dB�XB�RB�LB�?B�FB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�3B�9B�9B�LB�qBBĜBƨBĜBĜBĜBŢBŢBǮB��B��BǮBŢB��B��B��B�B�B�ZB�B�`B�fB�`B�ZB�BB�5B�/B�)B�;B�fB�B�B�B�B�B�B�B�B�sB�`B�fB�fB�ZB�;B�5B�5B�;B�BB�;B�;B�;B�ZB�mB�fB�B��B	B	%B	+B	DB	JB	JB	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	(�B	$�B	$�B	(�B	)�B	-B	9XB	@�B	J�B	K�B	L�B	J�B	K�B	N�B	N�B	P�B	R�B	XB	_;B	dZB	e`B	e`B	hsB	m�B	p�B	q�B	p�B	q�B	t�B	x�B	z�B	|�B	� B	�B	�%B	�1B	�7B	�DB	�PB	�VB	�bB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�FB	�LB	�LB	�LB	�XB	�dB	�jB	�qB	�qB	�wB	�wB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
%B
1B
1B
1B
1B
1B
1B
1B
	7B
1B
+B
%B
1B
1B
+B
1B
DB
JB
JB
JB
PB
VB
VB
VB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
{B
�B
�B
�B
#�B
.2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190535                              AO  ARCAADJP                                                                    20181005190535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190535  QCF$                G�O�G�O�G�O�8000            