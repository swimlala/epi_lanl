CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:01Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190501  20181005190501  5904950 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               
A   AO  6429                            2B  A   APEX                            7464                            062512                          846 @ף��X�1   @ף�y\�B@4��7Kƨ�c��S��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      
A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�33A�33A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B���B���B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D,��D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dv��Dwy�Dx  Dy��D�3�D�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��@��A�\A>�\A^�\A~�\A�G�A�z�A�z�A�{A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx
=B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B�B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9�\C;��C=��C?��CA��CC��CE��CG��CI��CK��CM�\CO��CQ��CS��CU��CW��CY��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D��Ds�D��Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#��D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,s�D,��D-z=D-�=D.z=D.�=D/z=D/�=D0s�D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6s�D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:s�D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DH �DH��DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DY��DZ �DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Das�Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Di �Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dms�Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Du �Du��Du�=Dvz=Dv��Dws�Dw�=Dy�D�0�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�v�A�x�A�z�A�v�A�v�A�x�A�t�A�p�A�v�A�jA�K�A�"�A�%A��A�ƨAȅA�n�A�33A�JA���A��/A���AǕ�AǅA�hsA�E�A�&�A��AƧ�A�^5A��A�ĜAô9A�l�A��+A��FA�z�A��`A�Q�A��#A�E�A���A�z�A�dZA�XA�M�A�?}A�$�A�S�A��A���A�E�A�Q�A�S�A�?}A�+A��#A�E�A��A�A�A���A���A��A�p�A�hsA�bNA�I�A��A�A�C�A�VA�=qA��yA�\)A���A�E�A�$�A�A���A�A��/A���A��RA��A���A���A���A�z�A�{A�hsA���A�%A���A�ZA�VA��A��9A���A�+A��FA�O�AK�A{ƨAup�Ao�AkAkK�Ak�AE%AD�!ADr�AC��AB��AA"�A@�\A>bA<^5A:�A81A5�TA4ZA3�-A333A1�
A0��A/��A-�A,�A+hsA)K�A(�A(M�A'�A'�7A'&�A&=qA%XA$1'A"n�A �uA�\A;dA��A�FAS�A�uA�AO�A?}A��AƨAG�A�TA�A��AVAA�PA%AdZA��A�`A�#A��A�A	oA�A��An�AbA�#A�PAl�A;dAVA��@�"�@��F@�=q@���@���@�5?@���@�1'@�t�@�(�@�K�@�+@�n�@�{@�`B@�l�@���@�~�@�p�@�I�@�(�@�F@畁@�S�@�"�@�=q@�9X@�I�@�&�@㝲@�$�@�{@أ�@�@�z�@�&�@�Q�@ˮ@�bN@�E�@�|�@���@�r�@�|�@��@ɡ�@ǶF@�r�@��/@�%@�/@�p�@�@�5?@�@�9X@ǥ�@�=q@���@�o@�C�@�-@��`@þw@�S�@��@¸R@�V@��^@�hs@�?}@��@��`@��@�  @��y@�5?@���@�7L@��@�|�@��y@�5?@��@���@��D@�  @�  @��P@��+@��T@��@���@�o@��R@�V@���@�x�@�G�@�?}@�?}@�?}@��j@�9X@��F@�"�@���@��\@�n�@�V@�-@���@�p�@��@���@��/@��`@��`@���@��u@�I�@�1@�ƨ@��@���@��@��!@�v�@���@�@���@��7@�x�@�x�@�x�@�x�@��@��7@���@��-@���@��@�&�@���@���@�o@���@���@���@��!@���@���@�~�@�M�@���@�/@�j@��P@�dZ@�o@��!@�M�@��T@��^@���@��7@�O�@�%@�%@�&�@�%@�Ĝ@�j@�b@���@�S�@�;d@��@�o@���@�n�@���@��-@���@��@�V@�9X@��;@�ƨ@���@�C�@�K�@�l�@�+@��H@�=q@��T@���@��^@���@�x�@�O�@�?}@��@�z�@�I�@��@��
@���@�@���@�5?@��T@��-@�?}@��@��`@�r�@�1'@�|�@���@�{@�{@�{@�J@�J@�@�@���@���@��@��@���@�@�&�@���@�Q�@��w@�@��@�^5@��#@���@��h@�p�@�hs@�`B@�?}@��@�V@��D@�1@��m@���@��w@�|�@�;d@�
=@���@�E�@��@���@��h@�X@���@���@�z�@�j@�bN@�Z@�(�@���@���@�|�@�C�@�+@�@��@��\@�E�@�5?@��@��h@�V@���@��u@�(�@��;@�ƨ@�|�@�S�@�"�@��@��H@�^5@�=q@��	@p��@`�O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�x�A�z�A�v�A�v�A�x�A�t�A�p�A�v�A�jA�K�A�"�A�%A��A�ƨAȅA�n�A�33A�JA���A��/A���AǕ�AǅA�hsA�E�A�&�A��AƧ�A�^5A��A�ĜAô9A�l�A��+A��FA�z�A��`A�Q�A��#A�E�A���A�z�A�dZA�XA�M�A�?}A�$�A�S�A��A���A�E�A�Q�A�S�A�?}A�+A��#A�E�A��A�A�A���A���A��A�p�A�hsA�bNA�I�A��A�A�C�A�VA�=qA��yA�\)A���A�E�A�$�A�A���A�A��/A���A��RA��A���A���A���A�z�A�{A�hsA���A�%A���A�ZA�VA��A��9A���A�+A��FA�O�AK�A{ƨAup�Ao�AkAkK�Ak�AE%AD�!ADr�AC��AB��AA"�A@�\A>bA<^5A:�A81A5�TA4ZA3�-A333A1�
A0��A/��A-�A,�A+hsA)K�A(�A(M�A'�A'�7A'&�A&=qA%XA$1'A"n�A �uA�\A;dA��A�FAS�A�uA�AO�A?}A��AƨAG�A�TA�A��AVAA�PA%AdZA��A�`A�#A��A�A	oA�A��An�AbA�#A�PAl�A;dAVA��@�"�@��F@�=q@���@���@�5?@���@�1'@�t�@�(�@�K�@�+@�n�@�{@�`B@�l�@���@�~�@�p�@�I�@�(�@�F@畁@�S�@�"�@�=q@�9X@�I�@�&�@㝲@�$�@�{@أ�@�@�z�@�&�@�Q�@ˮ@�bN@�E�@�|�@���@�r�@�|�@��@ɡ�@ǶF@�r�@��/@�%@�/@�p�@�@�5?@�@�9X@ǥ�@�=q@���@�o@�C�@�-@��`@þw@�S�@��@¸R@�V@��^@�hs@�?}@��@��`@��@�  @��y@�5?@���@�7L@��@�|�@��y@�5?@��@���@��D@�  @�  @��P@��+@��T@��@���@�o@��R@�V@���@�x�@�G�@�?}@�?}@�?}@��j@�9X@��F@�"�@���@��\@�n�@�V@�-@���@�p�@��@���@��/@��`@��`@���@��u@�I�@�1@�ƨ@��@���@��@��!@�v�@���@�@���@��7@�x�@�x�@�x�@�x�@��@��7@���@��-@���@��@�&�@���@���@�o@���@���@���@��!@���@���@�~�@�M�@���@�/@�j@��P@�dZ@�o@��!@�M�@��T@��^@���@��7@�O�@�%@�%@�&�@�%@�Ĝ@�j@�b@���@�S�@�;d@��@�o@���@�n�@���@��-@���@��@�V@�9X@��;@�ƨ@���@�C�@�K�@�l�@�+@��H@�=q@��T@���@��^@���@�x�@�O�@�?}@��@�z�@�I�@��@��
@���@�@���@�5?@��T@��-@�?}@��@��`@�r�@�1'@�|�@���@�{@�{@�{@�J@�J@�@�@���@���@��@��@���@�@�&�@���@�Q�@��w@�@��@�^5@��#@���@��h@�p�@�hs@�`B@�?}@��@�V@��D@�1@��m@���@��w@�|�@�;d@�
=@���@�E�@��@���@��h@�X@���@���@�z�@�j@�bN@�Z@�(�@���@���@�|�@�C�@�+@�@��@��\@�E�@�5?@��@��h@�V@���@��u@�(�@��;@�ƨ@�|�@�S�@�"�@��@��H@�^5@�=q@��	@p��@`�O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�B"�B"�B"�B"�B"�B#�B#�B"�B#�B$�B%�B'�B(�B.B8RB<jBC�BF�BG�BF�BD�B@�B?}BA�BB�BK�BW
B^5BgmBv�B��B��B��B�B&�B5?BN�BR�B_;Br�By�Bz�B|�B}�B|�Bz�B}�B�1B|�By�B{�B}�B}�B}�B|�Bz�Bw�Bt�Bu�Bx�By�Bx�Bz�By�Bx�Bv�Bq�BcTBO�B�B��B�;B�wB��BgmB�B
�B
�5B
ƨB
�9B
��B
�{B
�uB
�uB
�uB
�uB
�\B
{�B
jB
hsB
k�B
hsB
`BB
S�B
L�B
H�B
F�B
>wB
6FB
.B
 �B
B	�B	�RB	�B	�B	hsB�ZB�TB�NB�;B�)B�B��B��B��BĜB�qB�^B�LB�FB�9B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�JB�+B�B�%B�=B�DB�=B�PB�JB�VB�oB��B��B�{B�hB�VB��B��B��B��B��B��B�hB�DB�7B�=B�=B�=B�=B�=B�7B�+B�B�Bz�Bx�B~�B�uB�oB��B��B�DB�B�+B�VB�VB�\B�oB��B��B��B�!BĜB��B��B��B��B��B��B�B�TB�B�B�B��BɺBƨB�LB�!B�-B�FB��B��B�;B�B�B�B�fB�B��B�;B�B�B�B��B��B��B	B	B	B	B	+B	hB	uB	hB	bB	�B	�B	�B	�B	�B	!�B	"�B	"�B	"�B	"�B	"�B	$�B	&�B	&�B	&�B	&�B	(�B	)�B	.B	1'B	2-B	1'B	6FB	7LB	6FB	7LB	<jB	>wB	?}B	@�B	A�B	B�B	D�B	G�B	I�B	J�B	K�B	L�B	R�B	R�B	W
B	\)B	\)B	\)B	^5B	_;B	_;B	aHB	hsB	k�B	m�B	n�B	o�B	o�B	o�B	p�B	t�B	x�B	y�B	{�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�1B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�3B	�?B	�RB	�XB	�XB	�^B	�jB	�wB	�wB	�wB	�wB	��B	��B	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
	7B
DB
JB
PB
PB
PB
PB
VB
VB
VB
VB
PB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
bB
bB
hB
(B
 'B
-]2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B"�B"�B"�B"�B"�B"�B#�B#�B"�B#�B$�B%�B'�B(�B.B8RB<jBC�BF�BG�BF�BD�B@�B?}BA�BB�BK�BW
B^5BgmBv�B��B��B��B�B&�B5?BN�BR�B_;Br�By�Bz�B|�B}�B|�Bz�B}�B�1B|�By�B{�B}�B}�B}�B|�Bz�Bw�Bt�Bu�Bx�By�Bx�Bz�By�Bx�Bv�Bq�BcTBO�B�B��B�;B�wB��BgmB�B
�B
�5B
ƨB
�9B
��B
�{B
�uB
�uB
�uB
�uB
�\B
{�B
jB
hsB
k�B
hsB
`BB
S�B
L�B
H�B
F�B
>wB
6FB
.B
 �B
B	�B	�RB	�B	�B	hsB�ZB�TB�NB�;B�)B�B��B��B��BĜB�qB�^B�LB�FB�9B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�JB�+B�B�%B�=B�DB�=B�PB�JB�VB�oB��B��B�{B�hB�VB��B��B��B��B��B��B�hB�DB�7B�=B�=B�=B�=B�=B�7B�+B�B�Bz�Bx�B~�B�uB�oB��B��B�DB�B�+B�VB�VB�\B�oB��B��B��B�!BĜB��B��B��B��B��B��B�B�TB�B�B�B��BɺBƨB�LB�!B�-B�FB��B��B�;B�B�B�B�fB�B��B�;B�B�B�B��B��B��B	B	B	B	B	+B	hB	uB	hB	bB	�B	�B	�B	�B	�B	!�B	"�B	"�B	"�B	"�B	"�B	$�B	&�B	&�B	&�B	&�B	(�B	)�B	.B	1'B	2-B	1'B	6FB	7LB	6FB	7LB	<jB	>wB	?}B	@�B	A�B	B�B	D�B	G�B	I�B	J�B	K�B	L�B	R�B	R�B	W
B	\)B	\)B	\)B	^5B	_;B	_;B	aHB	hsB	k�B	m�B	n�B	o�B	o�B	o�B	p�B	t�B	x�B	y�B	{�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�1B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�3B	�3B	�?B	�RB	�XB	�XB	�^B	�jB	�wB	�wB	�wB	�wB	��B	��B	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
	7B
DB
JB
PB
PB
PB
PB
VB
VB
VB
VB
PB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
bB
bB
hB
(B
 'B
-]2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190501                              AO  ARCAADJP                                                                    20181005190501    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190501  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190501  QCF$                G�O�G�O�G�O�C000            