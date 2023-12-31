CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:14Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190514  20181005190514  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               %A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׸��_C�1   @׸�O��@1��n���c���l�D1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      %A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B'��B0  B8  B?��BG��BP  BX  B`  Bh  Bp  Bw��B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D	y�D
  D
� D  D�fD  Dy�D��D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D�fDfD� D  D� D  D� D��D� D  Dy�D  D� D  Dy�D��D� D fD � D!  D!y�D"  D"�fD"��D#y�D#��D$� D%fD%�fD&fD&�fD'  D'� D(fD(� D)  D)�fD*fD*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D5��D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;y�D<  D<�fD=  D=� D>  D>� D?  D?�fD@fD@�fDA  DA� DB  DB� DB��DCy�DD  DD� DE  DEy�DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DVy�DV��DW� DXfDX�fDYfDY�fDZfDZ� D[fD[� D[��D\� D]  D]� D^  D^�fD_fD_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� DgfDg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr� Dr��Ds� Dt  Dt� Du  Duy�Dv  Dv�fDw  Dw� Dw�3Dyz=D�)�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B fgB(fgB0��B8��B@fgBHfgBP��BX��B`��Bh��Bp��BxfgBܙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp�Cr33Ct33Cv33Cx33Cz33C|33C~33C��C�&gC��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C�&gC��C��C��C��C��C�&gC��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�gD	gD	�gD
�D
��D�D�3D�D�gDgD��D�D�3D�D��D�D��D�D��D�D��D�D��D�D��D�D�gD�D��D�D�3D3D��D�D��D�D��DgD��D�D�gD�D��D�D�gDgD��D 3D ��D!�D!�gD"�D"�3D#gD#�gD$gD$��D%3D%�3D&3D&�3D'�D'��D(3D(��D)�D)�3D*3D*��D+gD+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0�3D1�D1��D2�D2�gD3�D3��D4�D4��D5�D5��D6gD6��D73D7�3D8�D8��D9�D9��D:�D:��D;�D;�gD<�D<�3D=�D=��D>�D>��D?�D?�3D@3D@�3DA�DA��DB�DB��DCgDC�gDD�DD��DE�DE�gDF�DF��DG�DG�gDH�DH��DI�DI��DJ�DJ��DKgDK��DL�DL��DM3DM��DN�DN��DO�DO��DP�DP�gDQ�DQ��DR3DR��DS�DS��DT�DT��DU�DU��DV�DV�gDWgDW��DX3DX�3DY3DY�3DZ3DZ��D[3D[��D\gD\��D]�D]��D^�D^�3D_3D_��D`�D`��DagDa��Db�Db��Dc�Dc��Dd�Dd��De�De��DfgDf��Dg3Dg��Dh�Dh��Di�Di��Dj3Dj��Dk�Dk��DlgDl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�3Dr3Dr��DsgDs��Dt�Dt��Du�Du�gDv�Dv�3Dw�Dw��Dw� Dy�
D�0RD�Å1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�v�A�v�A�v�A�x�A�x�A�|�A�|�A�x�A�|�A�z�A�z�A�z�A�z�A�~�AԍPAԍPAԍPAԋDAԋDAԋDAԋDAԍPAԏ\Aԏ\Aԏ\Aԏ\Aԏ\AԑhAԑhAԓuA�^5A��`A�  AŲ-AŋDA�bAħ�A�=qA�&�A�VA���A�
=A��HA�n�A�dZA���A�%A�VA��uA��HA��A�;dA�`BA���A�^5A�z�A��;A���A��/A��A�l�A��9A��9A�A�=qA��!A���A���A���A��A�M�A�&�A�%A�(�A�1'A�G�A�\)A�t�A�K�A�r�A�{A��A��mA�x�A�r�A�%A�I�A|��Aw��Ar��Aq�An�AmAj��AiG�AfȴAdr�Aa�^A_�mA]�hA[�AZQ�AX�AU/AP��AN�AM;dAKp�AJ�AH�AG��AE�ACC�AA�FAA%A@��A@�`A>�\A:��A9�7A8�RA6�DA3��A2��A1�A1XA.bNA,��A*��A*JA(�+A&�`A&-A$��A$��A#�A"A!VA �uA��AS�A1'A\)A1AG�AjA��A\)An�A�Ap�Ar�A{AbNA�A��A�A��A�HAr�A�AhsA
ĜA	p�A	;dA	33A	+A�A��A+A&�A%A�A��A1A��A�A=qAr�A�uAz�Av�A��A/A M�@���@��-@�J@��@��#@�?}@���@��`@�V@�l�@�  @�5?@���@�r�@��y@��@�o@��@�Ĝ@��@�V@�Ĝ@��@��H@�v�@�ff@�^@��@�I�@�ff@�7L@���@��m@߮@�^5@���@�@�O�@�z�@ڟ�@�^5@ӶF@���@ҧ�@ѩ�@��@�@͉7@Ϯ@�b@ϥ�@ύP@�dZ@�+@Η�@��@�X@̣�@˶F@��@���@�@�p�@Ĵ9@î@�@���@���@�p�@�`B@�%@� �@���@�
=@�ȴ@���@�5?@���@���@�/@���@��@��@��@��+@��R@�M�@�G�@���@�1@��;@�K�@�@�;d@��@�V@���@�@���@�x�@��@��@�I�@�A�@��F@�K�@�
=@��y@��!@�v�@�$�@���@��h@�`B@�O�@�O�@��9@�j@�bN@�z�@��;@�S�@��H@�
=@�K�@��@�I�@��D@��D@�I�@�b@�9X@���@�Ĝ@�r�@��@�&�@�&�@�?}@���@���@�@���@�v�@��\@��+@���@���@���@�^5@��7@��m@��@���@���@�5?@��D@���@���@�p�@���@��@���@�I�@��@��P@�|�@�l�@�C�@�"�@�
=@��@��+@���@�x�@�X@�&�@��@��`@���@���@�dZ@�+@�n�@�$�@��@��@�{@��@��h@�O�@���@�(�@�9X@�1@�1@��m@��;@���@���@���@�&�@���@��j@�I�@�(�@��w@�dZ@�33@�o@��H@��!@���@���@�~�@���@���@���@�9X@��@���@��@�dZ@�+@��y@���@��9@�j@���@��@�@�-@��@���@��@���@�O�@�1@��P@�S�@��@�V@�@���@��h@��@���@�b@�33@�
=@��H@��@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@��R@���@�V@�5?@��^@��9@�r�@� �@���@�C�@��@���@�ȴ@��+@�^5@�M�@�E�@�J@��7@��@�V@��/@�Ĝ@���@��
@�\)@�
=@���@��\@�H@���@s/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�v�A�v�A�x�A�x�A�|�A�|�A�x�A�|�A�z�A�z�A�z�A�z�A�~�AԍPAԍPAԍPAԋDAԋDAԋDAԋDAԍPAԏ\Aԏ\Aԏ\Aԏ\Aԏ\AԑhAԑhAԓuA�^5A��`A�  AŲ-AŋDA�bAħ�A�=qA�&�A�VA���A�
=A��HA�n�A�dZA���A�%A�VA��uA��HA��A�;dA�`BA���A�^5A�z�A��;A���A��/A��A�l�A��9A��9A�A�=qA��!A���A���A���A��A�M�A�&�A�%A�(�A�1'A�G�A�\)A�t�A�K�A�r�A�{A��A��mA�x�A�r�A�%A�I�A|��Aw��Ar��Aq�An�AmAj��AiG�AfȴAdr�Aa�^A_�mA]�hA[�AZQ�AX�AU/AP��AN�AM;dAKp�AJ�AH�AG��AE�ACC�AA�FAA%A@��A@�`A>�\A:��A9�7A8�RA6�DA3��A2��A1�A1XA.bNA,��A*��A*JA(�+A&�`A&-A$��A$��A#�A"A!VA �uA��AS�A1'A\)A1AG�AjA��A\)An�A�Ap�Ar�A{AbNA�A��A�A��A�HAr�A�AhsA
ĜA	p�A	;dA	33A	+A�A��A+A&�A%A�A��A1A��A�A=qAr�A�uAz�Av�A��A/A M�@���@��-@�J@��@��#@�?}@���@��`@�V@�l�@�  @�5?@���@�r�@��y@��@�o@��@�Ĝ@��@�V@�Ĝ@��@��H@�v�@�ff@�^@��@�I�@�ff@�7L@���@��m@߮@�^5@���@�@�O�@�z�@ڟ�@�^5@ӶF@���@ҧ�@ѩ�@��@�@͉7@Ϯ@�b@ϥ�@ύP@�dZ@�+@Η�@��@�X@̣�@˶F@��@���@�@�p�@Ĵ9@î@�@���@���@�p�@�`B@�%@� �@���@�
=@�ȴ@���@�5?@���@���@�/@���@��@��@��@��+@��R@�M�@�G�@���@�1@��;@�K�@�@�;d@��@�V@���@�@���@�x�@��@��@�I�@�A�@��F@�K�@�
=@��y@��!@�v�@�$�@���@��h@�`B@�O�@�O�@��9@�j@�bN@�z�@��;@�S�@��H@�
=@�K�@��@�I�@��D@��D@�I�@�b@�9X@���@�Ĝ@�r�@��@�&�@�&�@�?}@���@���@�@���@�v�@��\@��+@���@���@���@�^5@��7@��m@��@���@���@�5?@��D@���@���@�p�@���@��@���@�I�@��@��P@�|�@�l�@�C�@�"�@�
=@��@��+@���@�x�@�X@�&�@��@��`@���@���@�dZ@�+@�n�@�$�@��@��@�{@��@��h@�O�@���@�(�@�9X@�1@�1@��m@��;@���@���@���@�&�@���@��j@�I�@�(�@��w@�dZ@�33@�o@��H@��!@���@���@�~�@���@���@���@�9X@��@���@��@�dZ@�+@��y@���@��9@�j@���@��@�@�-@��@���@��@���@�O�@�1@��P@�S�@��@�V@�@���@��h@��@���@�b@�33@�
=@��H@��@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@��R@���@�V@�5?@��^@��9@�r�@� �@���@�C�@��@���@�ȴ@��+@�^5@�M�@�E�@�J@��7@��@�V@��/@�Ĝ@���@��
@�\)@�
=@���@��\@�H@���@s/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B��B��B��B1B�B>wBL�BS�BJ�B]/Bp�B~�B� B�B�hB��B��B��B�B�!B�?B��BB��B�wB�qB�RB�B��Bs�BB�B\B��BɺBŢB�}B�B��By�BVBA�B�B
�B
�mB
�
B
�!B
{�B
w�B
v�B
w�B
w�B
p�B
k�B
T�B
6FB
�B

=B
1B	�B	��B	��B	��B	�{B	�%B	�1B	w�B	hsB	[#B	M�B	A�B	9XB	,B	!�B	\B	B��B�B�B�B�mB�TB�;B�B��B��B��B��BǮB�jB�XB�FB�!B�B�B�B�B�!B��B�BB�qB�qBĜBɺBȴBƨBŢB�wB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�B�3B�B��B��B��B��B��B��B��B��B��B�B�3B�RBƨB��B��B�#B�B��B	B	B	DB	\B	bB	hB	PB	B��B	  B	  B	B	1B	bB	{B	uB	{B	�B	�B	<jB	/B	&�B	<jB	D�B	@�B	<jB	5?B	/B	#�B	�B	�B	{B	�B	�B	�B	�B	 �B	 �B	 �B	�B	�B	%�B	(�B	'�B	%�B	$�B	$�B	"�B	!�B	�B	PB	1B		7B	DB	DB	%B��B��B	PB	�B	{B	uB	uB	oB	bB	VB	PB	
=B	1B	%B	B��B��B��B��B��B��B��B��B��B	  B	B	B	%B	1B		7B	PB	bB	�B	�B	�B	�B	�B	�B	�B	&�B	'�B	#�B	$�B	&�B	)�B	,B	/B	49B	8RB	=qB	=qB	A�B	A�B	A�B	E�B	F�B	G�B	H�B	J�B	L�B	M�B	N�B	O�B	Q�B	S�B	VB	YB	\)B	aHB	jB	iyB	jB	m�B	o�B	p�B	p�B	o�B	r�B	u�B	z�B	�B	�B	�B	�B	�1B	�DB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�XB	�jB	�XB	�FB	�-B	�B	��B	�B	�jB	ŢB	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�5B	�5B	�5B	�;B	�5B	�5B	�5B	�/B	�BB	�`B	�fB	�fB	�fB	�`B	�`B	�ZB	�NB	�TB	�fB	�sB	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
�B
OB
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B��B��B��B1B�B>wBL�BS�BJ�B]/Bp�B~�B� B�B�hB��B��B��B�B�!B�?B��BB��B�wB�qB�RB�B��Bs�BB�B\B��BɺBŢB�}B�B��By�BVBA�B�B
�B
�mB
�
B
�!B
{�B
w�B
v�B
w�B
w�B
p�B
k�B
T�B
6FB
�B

=B
1B	�B	��B	��B	��B	�{B	�%B	�1B	w�B	hsB	[#B	M�B	A�B	9XB	,B	!�B	\B	B��B�B�B�B�mB�TB�;B�B��B��B��B��BǮB�jB�XB�FB�!B�B�B�B�B�!B��B�BB�qB�qBĜBɺBȴBƨBŢB�wB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�B�3B�B��B��B��B��B��B��B��B��B��B�B�3B�RBƨB��B��B�#B�B��B	B	B	DB	\B	bB	hB	PB	B��B	  B	  B	B	1B	bB	{B	uB	{B	�B	�B	<jB	/B	&�B	<jB	D�B	@�B	<jB	5?B	/B	#�B	�B	�B	{B	�B	�B	�B	�B	 �B	 �B	 �B	�B	�B	%�B	(�B	'�B	%�B	$�B	$�B	"�B	!�B	�B	PB	1B		7B	DB	DB	%B��B��B	PB	�B	{B	uB	uB	oB	bB	VB	PB	
=B	1B	%B	B��B��B��B��B��B��B��B��B��B	  B	B	B	%B	1B		7B	PB	bB	�B	�B	�B	�B	�B	�B	�B	&�B	'�B	#�B	$�B	&�B	)�B	,B	/B	49B	8RB	=qB	=qB	A�B	A�B	A�B	E�B	F�B	G�B	H�B	J�B	L�B	M�B	N�B	O�B	Q�B	S�B	VB	YB	\)B	aHB	jB	iyB	jB	m�B	o�B	p�B	p�B	o�B	r�B	u�B	z�B	�B	�B	�B	�B	�1B	�DB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�XB	�jB	�XB	�FB	�-B	�B	��B	�B	�jB	ŢB	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�5B	�5B	�5B	�;B	�5B	�5B	�5B	�/B	�BB	�`B	�fB	�fB	�fB	�`B	�`B	�ZB	�NB	�TB	�fB	�sB	�B	�B	�B	�B	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
�B
OB
$�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190514                              AO  ARCAADJP                                                                    20181005190514    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190514  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190514  QCF$                G�O�G�O�G�O�8000            