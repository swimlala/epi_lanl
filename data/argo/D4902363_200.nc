CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-17T00:35:14Z creation;2018-01-17T00:35:17Z conversion to V3.1;2019-12-19T07:51:36Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180117003514  20200115121519  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_200                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�E5���1   @�E6DDD�@; A�7K��dTm��8�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCs�DC��DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}Dֹ�D��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��RD��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D���D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A� �A� �A�(�A�+A�+A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�&�A�&�A�(�A�(�A��A�"�A�{A�
=A���A��wA��-A���A��#A��A��+A�A��uA���A�dZA��+A���A��mA���A��DA��TA�&�A�~�A�$�A�p�A���A�1A�t�A�VA�`BA�ĜA���A��DA��A��DA��HA�/A��A��+A�bNA�ȴA�%A�(�A�K�A���A�K�A��yA��A��A���A�S�A� �A��A�I�A�v�A���A�\)A���A��-A��\A�jA�=qA��A���A���A�;dA~~�A}�wA|�A|v�A{��AzI�Ay7LAxr�Aw�wAu��At��As��Aq`BAoƨAoG�Ao
=AnJAlbNAj��AiAi33Ah��Ag
=Af~�AeK�Ac��Ab��Ab9XAa�wA`��A_7LA^�uA^ZA^$�A]ƨA]+A\�9A\$�AZ��AYdZAY�AX�yAX��AW�^AW;dAVbNAUAR��AP �AN��AMG�AL-AJ�\AJ�AJ  AI�AI\)AH�jAHQ�AHbAGO�AF��AFr�AEO�ADJACp�AA�TAAS�AAC�AAVA@~�A>��A>1'A=G�A<�RA<1A;x�A;?}A:ZA9G�A9�A8�A7�A6��A5�A3�mA2r�A0��A0Q�A/��A.��A.E�A-\)A-&�A-%A,��A,��A,��A+33A*�DA)�wA)`BA(�HA'��A&��A%��A%A$ȴA$ffA"�/A!�wA ��A �HA ȴA A�A7LA�uA1'A  A��A7LA�A�AoAbNA�TA��A+A�AM�A��A��AS�A33A�!A{A�hA7LA%AbAA��A�!A�AƨA&�A�DA
�uA
A
1A
  A	�A	��AjA��A�A�!A(�A�hA�9A5?A�PA��A M�@�;d@�7L@�|�@���@���@���@���@�1@�@�R@�@�%@�9X@�1'@�1'@��@�@��y@��@��@��@���@��@�@�1@�x�@��
@��H@ڇ+@���@ם�@���@���@�A�@ѩ�@�j@��H@�E�@�V@�$�@͉7@�A�@�l�@ʗ�@ɡ�@�G�@�ƨ@�O�@�;d@§�@��-@��D@��@��@���@�x�@��@���@�z�@�A�@���@�dZ@�+@��@��-@�Z@���@��H@�-@�?}@���@��@��j@���@���@�ȴ@�ff@�J@�?}@��@�p�@��u@�z�@�  @��F@�t�@�E�@�x�@���@�9X@��;@�S�@��\@�=q@�{@�-@���@��@�%@���@���@�Ĝ@���@��@��@��D@��@���@�b@��#@�hs@�hs@��@���@�(�@�1@��@�$�@�/@��@�Ĝ@��j@��9@���@�j@�A�@��@��T@�p�@��/@� �@��F@�+@�@��H@��!@�^5@��@�/@���@�z�@�I�@� �@���@�\)@�C�@��@��@��R@�$�@�V@��D@�Q�@��@��@�S�@��H@��!@���@��\@�ff@�M�@��7@�z�@��m@��@�o@�=q@�^5@�M�@�{@��@���@��T@�@��-@��@�&�@�%@��9@�z�@�A�@�(�@��@���@��;@��m@��;@��;@�|�@�o@��@�@�@��@�ȴ@�^5@�J@�@��7@�%@���@���@���@��@��@��j@�z�@�A�@�@}@|��@{t�@z�H@y��@yhs@yX@y7L@y&�@x��@w��@wK�@vv�@v$�@u��@u�@u�@t�/@t�j@t�@t��@t�D@tz�@tI�@t(�@t1@t1@s��@s�@sC�@r~�@q��@q��@qhs@p �@o��@o�w@o�@o\)@n�y@nff@n5?@n{@l��@m/@m/@lz�@k�F@k��@k��@k@j^5@j^5@j-@j=q@j��@j��@j�H@j��@jM�@j�@i�@i�^@i�7@ihs@i�@h�`@hĜ@h�u@h1'@g�@g��@g�w@gl�@g
=@f�R@f5?@e��@e��@e��@e�-@e�h@e�@d�/@dZ@d1@c��@cC�@c33@co@b�!@b~�@bn�@b^5@b^5@b-@aG�@`��@`��@`�u@`�u@`�u@`�@`1'@_K�@^��@^V@^@]O�@\�/@\�D@\�@\1@\1@[�
@[t�@[@Z�H@Z�H@Z��@ZM�@Y�@Y�7@Yhs@YX@Y&�@X��@W��@W+@Vȴ@Vv�@VV@VV@VE�@V$�@V@U�@U�T@U��@U@U��@Up�@T��@T�D@T(�@S�
@S��@S33@R�\@R=q@R�@RJ@Q��@Q�@P��@P�@P1'@O��@O�@O��@O�@N��@N$�@M�-@M�h@Mp�@L��@Lz�@L1@K�m@K�
@K�F@Kt�@K"�@J�\@J^5@JM�@J�@I��@Ihs@IG�@I�@H��@H�`@H�`@H��@H��@H�u@H1'@G�@G
=@F��@Fv�@FV@F{@E�h@D��@C�
@CS�@B��@Bn�@A�@A�7@Ahs@A7L@A%@@Ĝ@@��@@bN@@1'@@b@?�;@?�@?;d@>�y@>ȴ@>�@>�R@>V@=��@=�h@<��@<��@<�j@<j@;ƨ@;t�@;33@:��@:=q@:-@9�@9��@9��@9��@9�7@9x�@9X@9G�@9&�@9�@9�@9%@9%@8��@8r�@7�;@7l�@7+@7
=@6�@6��@6�+@6E�@6@5`B@5V@4��@4��@4�/@4�@4��@4z�@4Z@49X@3��@3�@3�@3t�@3dZ@3"�@2��@2^5@2-@2J@1��@1��@1X@1&�@0��@0��@0A�@01'@/�@/��@/��@/�w@/|�@/K�@/+@/
=@.��@.�y@.�y@.ȴ@.�R@.��@.v�@.5?@-@-�h@-p�@-`B@-?}@,��@,�j@,�D@+��@+�
@+�F@+�@+C�@*�!@*M�@*=q@*=q@*�@)��@)hs@)&�@(�`@(bN@(b@(  @'�@'�P@'
=@&��@&��@&�@&�R@&ff@&@%��@%?}@%�@%V@$�@$�/@$�D@$(�@#��@#�F@#C�@#33@#"�@"�H@"�!@"�!@"^5@!��@!��@!%@ Ĝ@ ��@ 1'@�;@|�@K�@ȴ@�+@�@��@`B@O�@V@��@��@�D@j@9X@(�@(�@�@1@��@S�@S�@33@@�!@~�@M�@��@�^@�7@G�@�@%@�`@��@�9@�@bN@Q�@b@��@�P@l�@+@��@�+@ff@V@V@@O�@�D@�m@�
@�F@��@��@"�@��@n�@^5@-@�@�@�#@7L@Ĝ@�9@b@�;@�w@|�@+@
=@
=@��@�y@�@ȴ@�+@E�@5?@5?@5?@5?@$�@�@��@�-@�h@p�@?}@�@�@�@�D@Z@(�@�
@�F@��@�@dZ@S�@o@
��@
~�@
n�@
n�@
n�@
^5@
�@	�@	�^@	��@	x�@	X@	hs@	hs@	hs@	hs@	hs@	G�@	G�@	G�@	G�@	G�@	G�@	7L@	�@�`@�`@�9@�@Q�@Q�@bN@Q�@ �@  @�@�;@�;@��@��@��@�P@|�@\)@K�@K�@;d@+@�@��@�@��@v�@V@5?@{@�h@/@V@��@�@�/@�/@�j@�@�D@z�@j@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�"�A� �A� �A�(�A�+A�+A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�&�A�&�A�(�A�(�A��A�"�A�{A�
=A���A��wA��-A���A��#A��A��+A�A��uA���A�dZA��+A���A��mA���A��DA��TA�&�A�~�A�$�A�p�A���A�1A�t�A�VA�`BA�ĜA���A��DA��A��DA��HA�/A��A��+A�bNA�ȴA�%A�(�A�K�A���A�K�A��yA��A��A���A�S�A� �A��A�I�A�v�A���A�\)A���A��-A��\A�jA�=qA��A���A���A�;dA~~�A}�wA|�A|v�A{��AzI�Ay7LAxr�Aw�wAu��At��As��Aq`BAoƨAoG�Ao
=AnJAlbNAj��AiAi33Ah��Ag
=Af~�AeK�Ac��Ab��Ab9XAa�wA`��A_7LA^�uA^ZA^$�A]ƨA]+A\�9A\$�AZ��AYdZAY�AX�yAX��AW�^AW;dAVbNAUAR��AP �AN��AMG�AL-AJ�\AJ�AJ  AI�AI\)AH�jAHQ�AHbAGO�AF��AFr�AEO�ADJACp�AA�TAAS�AAC�AAVA@~�A>��A>1'A=G�A<�RA<1A;x�A;?}A:ZA9G�A9�A8�A7�A6��A5�A3�mA2r�A0��A0Q�A/��A.��A.E�A-\)A-&�A-%A,��A,��A,��A+33A*�DA)�wA)`BA(�HA'��A&��A%��A%A$ȴA$ffA"�/A!�wA ��A �HA ȴA A�A7LA�uA1'A  A��A7LA�A�AoAbNA�TA��A+A�AM�A��A��AS�A33A�!A{A�hA7LA%AbAA��A�!A�AƨA&�A�DA
�uA
A
1A
  A	�A	��AjA��A�A�!A(�A�hA�9A5?A�PA��A M�@�;d@�7L@�|�@���@���@���@���@�1@�@�R@�@�%@�9X@�1'@�1'@��@�@��y@��@��@��@���@��@�@�1@�x�@��
@��H@ڇ+@���@ם�@���@���@�A�@ѩ�@�j@��H@�E�@�V@�$�@͉7@�A�@�l�@ʗ�@ɡ�@�G�@�ƨ@�O�@�;d@§�@��-@��D@��@��@���@�x�@��@���@�z�@�A�@���@�dZ@�+@��@��-@�Z@���@��H@�-@�?}@���@��@��j@���@���@�ȴ@�ff@�J@�?}@��@�p�@��u@�z�@�  @��F@�t�@�E�@�x�@���@�9X@��;@�S�@��\@�=q@�{@�-@���@��@�%@���@���@�Ĝ@���@��@��@��D@��@���@�b@��#@�hs@�hs@��@���@�(�@�1@��@�$�@�/@��@�Ĝ@��j@��9@���@�j@�A�@��@��T@�p�@��/@� �@��F@�+@�@��H@��!@�^5@��@�/@���@�z�@�I�@� �@���@�\)@�C�@��@��@��R@�$�@�V@��D@�Q�@��@��@�S�@��H@��!@���@��\@�ff@�M�@��7@�z�@��m@��@�o@�=q@�^5@�M�@�{@��@���@��T@�@��-@��@�&�@�%@��9@�z�@�A�@�(�@��@���@��;@��m@��;@��;@�|�@�o@��@�@�@��@�ȴ@�^5@�J@�@��7@�%@���@���@���@��@��@��j@�z�@�A�@�@}@|��@{t�@z�H@y��@yhs@yX@y7L@y&�@x��@w��@wK�@vv�@v$�@u��@u�@u�@t�/@t�j@t�@t��@t�D@tz�@tI�@t(�@t1@t1@s��@s�@sC�@r~�@q��@q��@qhs@p �@o��@o�w@o�@o\)@n�y@nff@n5?@n{@l��@m/@m/@lz�@k�F@k��@k��@k@j^5@j^5@j-@j=q@j��@j��@j�H@j��@jM�@j�@i�@i�^@i�7@ihs@i�@h�`@hĜ@h�u@h1'@g�@g��@g�w@gl�@g
=@f�R@f5?@e��@e��@e��@e�-@e�h@e�@d�/@dZ@d1@c��@cC�@c33@co@b�!@b~�@bn�@b^5@b^5@b-@aG�@`��@`��@`�u@`�u@`�u@`�@`1'@_K�@^��@^V@^@]O�@\�/@\�D@\�@\1@\1@[�
@[t�@[@Z�H@Z�H@Z��@ZM�@Y�@Y�7@Yhs@YX@Y&�@X��@W��@W+@Vȴ@Vv�@VV@VV@VE�@V$�@V@U�@U�T@U��@U@U��@Up�@T��@T�D@T(�@S�
@S��@S33@R�\@R=q@R�@RJ@Q��@Q�@P��@P�@P1'@O��@O�@O��@O�@N��@N$�@M�-@M�h@Mp�@L��@Lz�@L1@K�m@K�
@K�F@Kt�@K"�@J�\@J^5@JM�@J�@I��@Ihs@IG�@I�@H��@H�`@H�`@H��@H��@H�u@H1'@G�@G
=@F��@Fv�@FV@F{@E�h@D��@C�
@CS�@B��@Bn�@A�@A�7@Ahs@A7L@A%@@Ĝ@@��@@bN@@1'@@b@?�;@?�@?;d@>�y@>ȴ@>�@>�R@>V@=��@=�h@<��@<��@<�j@<j@;ƨ@;t�@;33@:��@:=q@:-@9�@9��@9��@9��@9�7@9x�@9X@9G�@9&�@9�@9�@9%@9%@8��@8r�@7�;@7l�@7+@7
=@6�@6��@6�+@6E�@6@5`B@5V@4��@4��@4�/@4�@4��@4z�@4Z@49X@3��@3�@3�@3t�@3dZ@3"�@2��@2^5@2-@2J@1��@1��@1X@1&�@0��@0��@0A�@01'@/�@/��@/��@/�w@/|�@/K�@/+@/
=@.��@.�y@.�y@.ȴ@.�R@.��@.v�@.5?@-@-�h@-p�@-`B@-?}@,��@,�j@,�D@+��@+�
@+�F@+�@+C�@*�!@*M�@*=q@*=q@*�@)��@)hs@)&�@(�`@(bN@(b@(  @'�@'�P@'
=@&��@&��@&�@&�R@&ff@&@%��@%?}@%�@%V@$�@$�/@$�D@$(�@#��@#�F@#C�@#33@#"�@"�H@"�!@"�!@"^5@!��@!��@!%@ Ĝ@ ��@ 1'@�;@|�@K�@ȴ@�+@�@��@`B@O�@V@��@��@�D@j@9X@(�@(�@�@1@��@S�@S�@33@@�!@~�@M�@��@�^@�7@G�@�@%@�`@��@�9@�@bN@Q�@b@��@�P@l�@+@��@�+@ff@V@V@@O�@�D@�m@�
@�F@��@��@"�@��@n�@^5@-@�@�@�#@7L@Ĝ@�9@b@�;@�w@|�@+@
=@
=@��@�y@�@ȴ@�+@E�@5?@5?@5?@5?@$�@�@��@�-@�h@p�@?}@�@�@�@�D@Z@(�@�
@�F@��@�@dZ@S�@o@
��@
~�@
n�@
n�@
n�@
^5@
�@	�@	�^@	��@	x�@	X@	hs@	hs@	hs@	hs@	hs@	G�@	G�@	G�@	G�@	G�@	G�@	7L@	�@�`@�`@�9@�@Q�@Q�@bN@Q�@ �@  @�@�;@�;@��@��@��@�P@|�@\)@K�@K�@;d@+@�@��@�@��@v�@V@5?@{@�h@/@V@��@�@�/@�/@�j@�@�D@z�@j@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B\BDBB�B�B��B��BŢBB�qB��B�dBĜB��BǮBB�3B��B�\B�Br�BaHBI�B(�BuBhB  B��B��B��B�JBiyBXBD�BQ�BE�B=qB5?B$�B�B�BBDB1BB  B
��B
��B
�B
�5B
�jB
�-B
�}B
�}B
�dB
�jB
�^B
�?B
�'B
��B
��B
� B
r�B
y�B
s�B
s�B
m�B
^5B
W
B
R�B
M�B
;dB
33B
.B
�B
�B
�B
�B
bB
B	��B	��B	��B	��B	�fB	�fB	�/B	��B	��B	��B	��B	ÖB	�^B	�^B	�qB	�dB	�LB	�!B	�B	��B	��B	�\B	��B	�{B	�hB	�7B	�B	}�B	q�B	e`B	Q�B	O�B	K�B	I�B	>wB	F�B	D�B	@�B	C�B	?}B	=qB	>wB	8RB	8RB	33B	+B	#�B	#�B	�B	�B	�B	�B	uB	+B	B��B��B��B��B��B�B�yB�B�yB�BB�#B��BɺBǮB��BĜB��B�}B�jB�^B�qB�}B�qB�dB�FB�B�B�B�B��B��B��B��B��B��B��B�DB�7B�JB�\B�VB�1B�B�B�B�B�B~�B}�Bv�Bt�Bt�Bt�Bt�Br�Bp�Bp�Bo�Bn�Bm�Bm�BiyBe`BffBdZBbNBZBT�BP�BN�BM�BG�BL�BJ�BA�BJ�BT�BS�BR�BO�BH�BH�BL�BJ�BI�BF�BD�BD�B@�B5?B49B49B,B'�B�B�B&�B'�B(�B(�B#�B�B�B!�B%�B$�B �B�B�B%�B!�B �B�B�B�B�B�B�B�B�B �B�B!�B�B�BoB�B�B�B#�B(�B(�B&�B&�B%�B#�B"�B�B�B�B!�B �B�B!�B$�B#�B'�B(�B+B-B,B+B,B+B'�B)�B'�B+B.B-B.B+B-B2-B8RB;dB;dB=qB>wB<jB>wBD�BI�BO�BQ�BS�BT�BQ�BT�BXBW
BZBYB[#BcTBffBiyBjBl�Bn�Bs�Bx�By�By�Bz�B{�B�7B�\B�{B�oB�JB�hB��B��B��B��B��B��B��B��B�B�-B�?B�FB�FB�FB�LB�FB�LBƨBȴB��B��B��B�
B�B�B�B�B�B�/B�BB�HB�HB�BB�NB�`B�fB�`B�`B�ZB�`B�B�B�B��B��B��B��B��B��B��B��B��B	B	PB	{B	�B	�B	"�B	$�B	%�B	'�B	(�B	)�B	)�B	+B	+B	+B	.B	0!B	33B	49B	5?B	6FB	:^B	>wB	A�B	B�B	C�B	B�B	E�B	H�B	K�B	M�B	M�B	L�B	K�B	K�B	K�B	L�B	K�B	O�B	Q�B	R�B	R�B	S�B	T�B	S�B	S�B	R�B	R�B	VB	VB	XB	YB	[#B	^5B	^5B	^5B	]/B	\)B	cTB	gmB	n�B	q�B	u�B	v�B	y�B	{�B	}�B	}�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�DB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�?B	�LB	�XB	�^B	�^B	�dB	�jB	�jB	�wB	�wB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�/B	�)B	�)B	�BB	�HB	�NB	�NB	�ZB	�`B	�fB	�sB	�sB	�mB	�mB	�sB	�yB	�B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
B
B
B
+B
1B
1B
1B
1B

=B
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
\B
\B
\B
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
�B
 �B
!�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
-B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
49B
5?B
5?B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
;dB
:^B
;dB
<jB
=qB
=qB
>wB
?}B
?}B
>wB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
F�B
E�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
T�B
XB
XB
W
B
W
B
VB
VB
XB
YB
YB
YB
YB
YB
XB
XB
YB
XB
ZB
ZB
ZB
ZB
[#B
\)B
[#B
\)B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�tBۦB͟B��B��B��B�.B��B�0B��B��B�KB�-B��B�zB��B�9Bt�Bc:BL~B-�B9B[B�B��B�EB�/B��Bm�B\)BIBSuBHB?B7B'B�BkB�B0B	7BB �B
��B
�lB
�B
�vB
��B
��B
��B
�OB
�B
��B
��B
��B
��B
��B
��B
��B
t�B
z�B
t�B
tB
n�B
`'B
XEB
TB
N�B
=�B
4�B
/�B
dB
CB
IB
xB
�B
B	��B	�0B	��B	��B	�B	�RB	��B	��B	��B	ѷB	̘B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�KB	��B	��B	��B	�B	�XB	�%B	cB	s�B	hXB	U2B	Q�B	M�B	K^B	@OB	F�B	ESB	A;B	C�B	@iB	>B	?B	9>B	8�B	4B	,�B	%`B	$�B	]B	QB	�B	1B	{B		B	-B�B��B��B��B�lB��B��B��B�KB�BܒB��B��BɆB�GBŢBªB�iB�qB�dB��B��B��B��B�B��B� B�)B��B��B�&B�'B�7B�eB�#B�mB�6B��B�6B��B��B�7B�{B��B��B��B��B�B~�Bx8BvBu�ButBuZBs�Bq[BqABp;Bo5Bn/Bm�BjeBffBgBd�Bb�B[�BV�BR�BPHBO�BI�BM�BLBC�BKxBT�BTFBS@BP�BJrBI�BM�BK�BJ�BG�BE�BE�BA�B7�B5�B5ZB-�B)_B�B�B'�B(�B)�B)�B%B�B7B"hB&2B%B!|B)B1B&B"�B!HB�B�B�B�B+B�B�B \B!�B �B"�B�B�B,B�B�B5B$B)DB)�B'�B'�B&�B$�B#�BBsB�B"hB!�B �B"�B%zB$�B(XB)_B+QB-]B,qB+�B,WB+�B(�B*B(�B+�B.�B-�B.�B,=B./B3B8�B;�B<B=�B>�B=qB?�BFBJXBPBRoBTaBU�BR�BU�BXyBW�BZ�BY�B[�Bc�Bf�Bi�Bj�Bl�BoBs�Bx�By�By�B{B|PB�7B��B�{B�&B��B��B��B��B��B�
B�B��B�jB��B�5B�aB�tB�`B��B��B��B�B��B��B�7B�^B�NB�uB�YB�_B�eB�eBٚBڠBݘB��B�B�|B��B�B�zB�B��B��B�B�LB�B��B�B�	B�DB�PB�BB�.B�.B�HB�HB��B	�B	�B	�B	B	7B	"�B	%B	&B	(
B	)B	*B	*B	+B	+6B	+kB	.cB	0oB	3hB	4�B	5tB	6�B	:^B	>]B	A�B	B�B	C�B	B�B	E�B	H�B	K�B	M�B	NB	MB	LB	LB	LB	MB	LJB	O�B	RB	R�B	SB	TB	U2B	T,B	TFB	SuB	S�B	VmB	VmB	X_B	YeB	[WB	^OB	^jB	^jB	]dB	\�B	c�B	g�B	n�B	q�B	u�B	wB	zB	|B	~(B	~(B	.B	.B	B	� B	� B	�'B	�;B	�UB	�AB	�oB	�aB	�3B	�MB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�&B	�,B	��B	�*B	�>B	�B	�"B	�/B	�!B	�'B	�GB	�hB	�ZB	�fB	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�(B	�B	�B	�,B	�9B	�B	�?B	�?B	�KB	�B	�B	�1B	�eB	�yB	�WB	�dB	�5B	�5B	�OB	�dB	�xB	�xB	�vB	�|B	�B	�B	�B	�zB	�B	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�*B	�B	�6B	�B	�B	�BB
 B
 4B	�.B	�.B
 OB
;B
AB
'B
AB
-B
MB
?B
YB
?B
SB
SB
mB
_B
KB
fB
fB
fB

rB
^B
^B
JB
PB
�B
jB
jB
�B
�B
�B
�B
vB
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
�B
!B
!�B
#B
#�B
#�B
$�B
%B
$�B
%B
%B
%�B
'�B
(
B
(
B
($B
(�B
)B
*B
*0B
*0B
+B
-)B
.B
.IB
.IB
.cB
.IB
/OB
/OB
/OB
0;B
0UB
0UB
0UB
1AB
1[B
2aB
3MB
4TB
5?B
5ZB
4nB
5ZB
6zB
6`B
6FB
6`B
6FB
6zB
6`B
6zB
6zB
6`B
6�B
7fB
7fB
8RB
7fB
7fB
7�B
7�B
7�B
8�B
8�B
8lB
8lB
8�B
9�B
;B
;dB
;�B
:�B
;B
<�B
=�B
=�B
>�B
?}B
?�B
>�B
=�B
?�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
F�B
E�B
G�B
G�B
G�B
H�B
IB
I�B
IB
I�B
I�B
J�B
K�B
K�B
K�B
K�B
MB
MB
NB
M�B
M�B
M�B
M�B
M�B
MB
M�B
N�B
OB
OB
OB
O�B
N�B
O�B
O�B
QB
QB
RB
Q�B
R B
RB
R B
R B
R B
SB
R B
SB
S&B
S&B
S&B
S@B
T�B
UB
UB
U2B
TFB
T,B
TaB
U2B
X+B
X+B
W?B
W$B
VSB
VSB
XEB
Y1B
Y1B
Y1B
Y1B
YKB
XEB
XEB
Y1B
X_B
Z7B
Z7B
ZQB
Z7B
[WB
\CB
[WB
\)B
[=B
[=B
[WB
\CB
\)B
]IB
]/B
^5B
^OB
^jB
_VB
_pB
_pB
_VB
_VB
`BB
`\B
_pB
_pB
`vB
`vB
`vB
a|B
abB
bNB
b�B
bhB
a|B
a�B
bhB
cTB
cTB
cnB
cnB
bhB
cnB
cnB
cnB
dtB
d�B
ezB
ezB
ezB
ezB
dtB
dtB
ezB
ezB
ezB
e`B
e`B
e`B
d�B
dtB
e`B
ezB
ezB
ezB
ffB
ffB
f�B
f�B
f�B
ffB
g�B
gmB
gmB
g�B
g�B
g�B
gmB
g�B
hsB
hsB
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
h�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801210034522018012100345220180121003452201806221236242018062212362420180622123624201804050432552018040504325520180405043255  JA  ARFMdecpA19c                                                                20180117093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180117003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180117003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180117003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180117003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180117003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180117003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180117003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180117003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180117003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180117005525                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180117153316  CV  JULD            G�O�G�O�F�)�                JM  ARCAJMQC2.0                                                                 20180120153452  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180120153452  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193255  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033624  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121519                      G�O�G�O�G�O�                