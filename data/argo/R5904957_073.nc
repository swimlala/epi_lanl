CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:18Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140818  20181024140818  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               IA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @���)V�
1   @���s�@26E�����c�$�/�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      IA   A   A   @�33@�  A   AffA@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��D   D � D  D� D  D� D  D� DfD� D  D� D  D� DfD� D  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D8��D9y�D9��D:y�D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� DmfDm�fDn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy�fD�O
D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�\)A�A"zAC�Ac�A��
A�
=A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B9Q�BAQ�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�BȨ�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C!GC:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6T{C8:�C:!GC<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^!GC`!GCb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~!GC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�*>C�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�*>C�*>D �D ��D�D��D�D��D�D��DD��D�D��D�D��DD��D�D��D	�D	��D
RD
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��DD�DD��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�RD$�D$��D%�D%��D&D&��D'�D'��D(�D(��D)D)��D*�D*��D+�D+��D,�D,�D-D-��D.�D.��D/�D/�D0�D0��D1�D1��D2�D2�D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�RD9RD9�RD:RD:�RD;RD;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA�RDB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG�RDH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQDQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�RDdRDd��DeDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��DjDj��Dk�Dk��Dl�Dl��DmDm�Dn�Dn�RDo�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�RDy�D�VfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�bNA�bNA�ffA�hsA�jA�n�A�p�A�p�A�z�A�|�A�t�A�t�A�p�A�t�A�t�A�p�A�jA�C�A�9XA��/A��Aܝ�A��`A�ĜAۋDA���A�ƨA���AؼjA���A�x�A���Aհ!A��`A��A�bA���A���A��A�  AɮA�33AȶFA��AƼjA�1'A��/A��A���AÝ�A�1'A�5?A�%A�x�A��A�A�A�33A���A��FA�z�A���A�\)A��jA�x�A�/A���A�JA�\)A���A�x�A�bA�JA�7LA�9XA���A���A�v�A�G�A���A�&�A��hA��TA�XA���A���A�7LA�`BA��^A�S�A��^A�+A�A�A�"�A�K�A��jA���A�JA��A��!A�-A��A���A��DA�oAK�A{�A{�AzM�AxĜAv^5At-AqS�Ak�PAf�uAb�A`E�A^�jA]O�AXn�AS�^AP��AOXAM7LAJr�AG�AFM�AE�AE�TAD�HAD�AB��AA�hA@z�A>��A>ĜA>~�A>9XA>$�A=l�A;XA:JA8�A7��A7oA6=qA4�HA2��A1�PA0�A/��A-��A-"�A+�A*�A*�/A)��A(�\A&�A%dZA%oA$�RA$�+A#x�A!33A�7AC�A=qA��A~�AbA��AdZA%A�\A�A�A��A��A�#A�PA�A��AVA�A1'A��A�
A�DAv�A��A�A	O�A=qAC�A��A��An�At�@���@�9X@��u@�S�@��y@��@���@�{@��+@��T@��P@��7@�Q�@��@�{@��@��9@���@�@���@�ff@�$�@���@��`@��m@�-@�@��@���@�O�@�Ĝ@�\@��T@��#@�hs@��@��@��@��`@�b@ۍP@��y@ف@�7L@��m@�hs@�  @�+@�M�@�X@�b@���@́@���@̓u@�b@˾w@˕�@��@��y@ǥ�@Ɨ�@Ɨ�@�ȴ@Ɵ�@�v�@�J@��m@�v�@�ff@�p�@�Z@���@�K�@�C�@�33@�n�@�&�@�1@���@�"�@��y@���@�hs@��9@� �@��m@��w@�|�@�dZ@��@���@�@�`B@���@�Ĝ@���@��/@��@��H@��@���@�/@���@�j@�1'@���@���@�dZ@�dZ@�"�@�J@��@���@��`@���@�Ĝ@��/@�Ĝ@��@�
=@��!@�^5@�J@���@�x�@�?}@���@��/@��`@��@��@�Q�@�b@���@�
=@��R@�=q@�J@�V@���@�@�v�@��#@��-@�`B@�%@��D@�I�@�ƨ@�\)@�o@�ȴ@�J@���@���@�O�@��@���@���@�hs@��7@�/@�%@�A�@�o@��y@�t�@�\)@�
=@��R@���@�~�@�v�@��@���@�%@��@�z�@� �@�ƨ@��P@�\)@�o@���@���@���@��\@�~�@�~�@��+@�v�@�ff@�$�@���@�x�@�p�@�p�@�O�@��@�%@��j@��9@���@���@���@���@��u@��D@��D@��@�j@�A�@��w@�l�@�33@�;d@��y@��@���@��+@�-@���@�X@��@�V@�V@��@��@��D@�bN@��@���@�C�@��H@��+@�-@�J@��@���@�`B@���@�Ĝ@��j@��j@��@��@�A�@��;@�dZ@�S�@�K�@�o@�-@�O�@�O�@�&�@��@�%@��`@��j@���@�z�@�j@��D@��@�7L@�X@�?}@���@���@��D@�Q�@�ƨ@���@���@��@�|�@�l�@�l�@�\)@�S�@�"�@��H@�^5@�J@��#@��h@��@��@��/@��@�j@�9X@�1@���@sn/@_��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�`BA�bNA�bNA�ffA�hsA�jA�n�A�p�A�p�A�z�A�|�A�t�A�t�A�p�A�t�A�t�A�p�A�jA�C�A�9XA��/A��Aܝ�A��`A�ĜAۋDA���A�ƨA���AؼjA���A�x�A���Aհ!A��`A��A�bA���A���A��A�  AɮA�33AȶFA��AƼjA�1'A��/A��A���AÝ�A�1'A�5?A�%A�x�A��A�A�A�33A���A��FA�z�A���A�\)A��jA�x�A�/A���A�JA�\)A���A�x�A�bA�JA�7LA�9XA���A���A�v�A�G�A���A�&�A��hA��TA�XA���A���A�7LA�`BA��^A�S�A��^A�+A�A�A�"�A�K�A��jA���A�JA��A��!A�-A��A���A��DA�oAK�A{�A{�AzM�AxĜAv^5At-AqS�Ak�PAf�uAb�A`E�A^�jA]O�AXn�AS�^AP��AOXAM7LAJr�AG�AFM�AE�AE�TAD�HAD�AB��AA�hA@z�A>��A>ĜA>~�A>9XA>$�A=l�A;XA:JA8�A7��A7oA6=qA4�HA2��A1�PA0�A/��A-��A-"�A+�A*�A*�/A)��A(�\A&�A%dZA%oA$�RA$�+A#x�A!33A�7AC�A=qA��A~�AbA��AdZA%A�\A�A�A��A��A�#A�PA�A��AVA�A1'A��A�
A�DAv�A��A�A	O�A=qAC�A��A��An�At�@���@�9X@��u@�S�@��y@��@���@�{@��+@��T@��P@��7@�Q�@��@�{@��@��9@���@�@���@�ff@�$�@���@��`@��m@�-@�@��@���@�O�@�Ĝ@�\@��T@��#@�hs@��@��@��@��`@�b@ۍP@��y@ف@�7L@��m@�hs@�  @�+@�M�@�X@�b@���@́@���@̓u@�b@˾w@˕�@��@��y@ǥ�@Ɨ�@Ɨ�@�ȴ@Ɵ�@�v�@�J@��m@�v�@�ff@�p�@�Z@���@�K�@�C�@�33@�n�@�&�@�1@���@�"�@��y@���@�hs@��9@� �@��m@��w@�|�@�dZ@��@���@�@�`B@���@�Ĝ@���@��/@��@��H@��@���@�/@���@�j@�1'@���@���@�dZ@�dZ@�"�@�J@��@���@��`@���@�Ĝ@��/@�Ĝ@��@�
=@��!@�^5@�J@���@�x�@�?}@���@��/@��`@��@��@�Q�@�b@���@�
=@��R@�=q@�J@�V@���@�@�v�@��#@��-@�`B@�%@��D@�I�@�ƨ@�\)@�o@�ȴ@�J@���@���@�O�@��@���@���@�hs@��7@�/@�%@�A�@�o@��y@�t�@�\)@�
=@��R@���@�~�@�v�@��@���@�%@��@�z�@� �@�ƨ@��P@�\)@�o@���@���@���@��\@�~�@�~�@��+@�v�@�ff@�$�@���@�x�@�p�@�p�@�O�@��@�%@��j@��9@���@���@���@���@��u@��D@��D@��@�j@�A�@��w@�l�@�33@�;d@��y@��@���@��+@�-@���@�X@��@�V@�V@��@��@��D@�bN@��@���@�C�@��H@��+@�-@�J@��@���@�`B@���@�Ĝ@��j@��j@��@��@�A�@��;@�dZ@�S�@�K�@�o@�-@�O�@�O�@�&�@��@�%@��`@��j@���@�z�@�j@��D@��@�7L@�X@�?}@���@���@��D@�Q�@�ƨ@���@���@��@�|�@�l�@�l�@�\)@�S�@�"�@��H@�^5@�J@��#@��h@��@��@��/@��@�j@�9X@�1@���@sn/@_��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�
B
�
B
�
B
�B
�
B
�B
�B
�
B
�B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�B �B2-B33B6FB?}B?}BI�BW
Bk�Bt�Bz�B�+B��B��B�-BB��B��BB+BhB�B-BO�BZBw�B�=B�DB�JB�VB�hB��B��B��B�B�'B�!B�XB�wB�'B�B��B��B�1Bu�Bp�Bp�Bn�Bn�Bo�Bu�Bz�Bv�Bo�Bn�Bn�Bn�BjB_;BVB:^B\B�B�
BÖB�LB�B�BO�B49B�BB
�sB
�#B
�LB
��B
��B
��B
��B
�oB
�\B
~�B
iyB
I�B
0!B
,B
$�B
�B
1B	��B	�;B	�B	�DB	t�B	e`B	`BB	Q�B	49B	�B	+B��B��B�B�fB�TB�ZB�fB�fB�mB�NB�5B�#B�
B��B��B��B��BȴB�jB�RB�FB�'B�!B�'B�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�PB�PB��B��B��B��B�?B�9B��B�oB�oB�BŢB�qB��B��B�JB�+B{�Bk�B\)BQ�BS�BbNBiyBv�B}�B�B�%B�{B��B��B��B��B��B��B�\B�VB�VB�\B�hB�{B��B��B��B�{B��B��B��B�B��B�B�!B�3B�LB�^B�jB�wB��B��B��B��B��BÖBǮB��B��B��B��B��B�B�/B�;B�HB�ZB�`B�`B�mB�fB�B�B�B��B��B��B��B��B��B	B	B	B	B	+B	+B	+B		7B	VB	uB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	+B	,B	-B	.B	0!B	2-B	49B	8RB	;dB	>wB	@�B	E�B	G�B	H�B	I�B	M�B	O�B	R�B	S�B	YB	\)B	]/B	^5B	^5B	aHB	bNB	bNB	bNB	cTB	ffB	iyB	l�B	s�B	w�B	{�B	~�B	�B	�B	�+B	�7B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�FB	�LB	�^B	�^B	�^B	�jB	�qB	�wB	��B	B	B	B	ĜB	ĜB	ŢB	ǮB	��B	��B	�
B	�B	�B	�B	�#B	�;B	�HB	�TB	�NB	�NB	�TB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
+B
+B
+B
1B
	7B
	7B
	7B
1B
1B
	7B

=B
JB
bB
hB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
*eB
<P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�
B
�
B
�
B
�B
�
B
�B
�B
�
B
�B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�B �B2-B33B6FB?}B?}BI�BW
Bk�Bt�Bz�B�+B��B��B�-BB��B��BB+BhB�B-BO�BZBw�B�=B�DB�JB�VB�hB��B��B��B�B�'B�!B�XB�wB�'B�B��B��B�1Bu�Bp�Bp�Bn�Bn�Bo�Bu�Bz�Bv�Bo�Bn�Bn�Bn�BjB_;BVB:^B\B�B�
BÖB�LB�B�BO�B49B�BB
�sB
�#B
�LB
��B
��B
��B
��B
�oB
�\B
~�B
iyB
I�B
0!B
,B
$�B
�B
1B	��B	�;B	�B	�DB	t�B	e`B	`BB	Q�B	49B	�B	+B��B��B�B�fB�TB�ZB�fB�fB�mB�NB�5B�#B�
B��B��B��B��BȴB�jB�RB�FB�'B�!B�'B�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�PB�PB��B��B��B��B�?B�9B��B�oB�oB�BŢB�qB��B��B�JB�+B{�Bk�B\)BQ�BS�BbNBiyBv�B}�B�B�%B�{B��B��B��B��B��B��B�\B�VB�VB�\B�hB�{B��B��B��B�{B��B��B��B�B��B�B�!B�3B�LB�^B�jB�wB��B��B��B��B��BÖBǮB��B��B��B��B��B�B�/B�;B�HB�ZB�`B�`B�mB�fB�B�B�B��B��B��B��B��B��B	B	B	B	B	+B	+B	+B		7B	VB	uB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	+B	,B	-B	.B	0!B	2-B	49B	8RB	;dB	>wB	@�B	E�B	G�B	H�B	I�B	M�B	O�B	R�B	S�B	YB	\)B	]/B	^5B	^5B	aHB	bNB	bNB	bNB	cTB	ffB	iyB	l�B	s�B	w�B	{�B	~�B	�B	�B	�+B	�7B	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�FB	�FB	�LB	�^B	�^B	�^B	�jB	�qB	�wB	��B	B	B	B	ĜB	ĜB	ŢB	ǮB	��B	��B	�
B	�B	�B	�B	�#B	�;B	�HB	�TB	�NB	�NB	�TB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
+B
+B
+B
1B
	7B
	7B
	7B
1B
1B
	7B

=B
JB
bB
hB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
*eB
<P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140818                              AO  ARCAADJP                                                                    20181024140818    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140818  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140818  QCF$                G�O�G�O�G�O�0               