CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-03-06T08:00:46Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20210306080046  20210306080046  5906101 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               AA   AO  7907                            2B  A   NAVIS_A                         1015                            170425                          863 @�cR�$�'1   @�cSl�@'š����d$�9Xb1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         AA   A   A   @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�|�D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z=q@��@��A��A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B�B���B���B���B���B���B���B�B���B�B���B���B���B���B���B���B���B�B���B���B���B㞹B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9�\C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D��D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}D��RD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�y�DݽD��D�=D�}D��RD��D�=D�}D߽D��D�=D�y�D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�O�A�S�A�ZA�ZA�XA�`BA�dZA�bNA�bNA�hsA�ffA�hsA�ffA�hsA�l�A�jA�n�A�l�A�n�A�r�A�n�A�r�A�n�A�t�A�v�A�z�A�v�A�z�A�|�A�~�AЁAЁAЁAЁAЁA�n�A� �A�bA�E�A�I�A�G�A�A�A��A��;A��+A���A���A�|�A�ffA��7A��
A�\)A��A���A�-A��A��A���A�Q�A��!A�hsAz$�AuO�Asx�AsAqC�Ao��AoAnn�AmXAkAh��Ae&�Ac"�Aa�FAaXA_\)A^1'A[ARVAK��AG�TAD�yA?�PA<=qA:��A: �A9��A8n�A6��A2(�A/oA.��A,��A,��A,I�A,5?A,bA*jA'�A&n�A$��A#��A$��A%`BA&r�A&�A&�jA&I�A&bA%��A%G�A%%A%oA%/A%S�A%\)A%G�A%G�A%;dA$�A$��A$��A$v�A$A"��A"�`A"ȴA"��A"Q�A!�FA!hsA ��A �+A �A�A7LA��A��A�A
=A��A�HA�\A1'A{A�^At�A+A��AJA\)A�`A�/A�Ar�A1'A�TAt�A��A�AVA�9A�uAE�A(�AJA�AƨA�hA?}A/A&�A�+A�A33A��An�A=qA9XA�A�#A�AC�A��A��AȴAn�AA�A{A�TA��AG�A�/A��Av�Ar�AbNA�+A�A�`A��A��A�+A^5A�TA33AĜAJA�
A�A�PAC�A�`Az�A$�A�7A?}A�A
�!A
�+A
  A	dZA��A��AE�A�;A�A�A^5A �A��AoA�RA�A^5A=qA5?A��A�A�yA�A��A7LA ��A M�@���@��+@�V@�E�@�5?@��-@�Q�@��@���@�X@�7L@���@�"�@�=q@�O�@���@���@�C�@�v�@�=q@��@�&�@��@��@���@�n�@��@�`B@���@�1@�@��@�5?@��T@�@陚@�7L@�Ĝ@� �@��m@�P@�+@��H@�R@�+@�V@�-@��@�V@���@�D@�@���@�n�@�5?@���@�p�@�&�@�Ĝ@��@ߥ�@ޏ\@݁@���@�{@١�@ف@�?}@���@�Ĝ@׮@ְ!@�$�@��#@պ^@�hs@���@�A�@�l�@��H@�5?@��T@љ�@�V@�Q�@ϥ�@�33@ΰ!@�-@�X@̼j@���@�"�@��H@���@ʗ�@ʏ\@�^5@�Ĝ@ǝ�@���@��@�7L@���@�1'@��m@ÍP@��y@�n�@��@�@��@��-@�hs@�V@��`@�z�@��@��P@�;d@���@���@�G�@��D@�A�@�l�@��+@�^5@��T@�O�@��`@�ƨ@���@�v�@��@�Ĝ@��;@�K�@���@�?}@���@��@��@�r�@�Z@�b@���@��P@�;d@�
=@��y@���@�V@�$�@��#@��-@��7@�`B@�O�@�?}@�7L@��@��@��/@�Ĝ@���@�A�@��
@�"�@���@���@��+@�~�@�J@��^@��7@�x�@��@��D@�Z@���@���@�t�@�K�@�o@���@�v�@��T@���@�@��h@�/@���@��@��@��@�"�@��+@�^5@�M�@���@��^@�hs@��@���@���@�r�@��m@�+@�ff@�$�@���@�x�@��@���@��@���@���@���@�V@��`@��j@�r�@�Q�@��@�|�@��@�ȴ@�E�@�O�@�Z@�ƨ@��y@�v�@��@���@��7@�V@��9@��@�Z@�I�@�1'@��@�K�@�v�@�$�@�J@��-@�hs@�`B@�`B@�X@�O�@�?}@��@��@���@���@�j@�Z@�I�@�1@���@�t�@�dZ@�K�@���@�V@�`B@�G�@��/@��9@�bN@�Q�@�A�@�(�@��@��@�+@��y@�ȴ@��!@��+@�V@�=q@�$�@�{@�J@���@���@���@�hs@��/@�Q�@�1@��m@��
@��w@���@�C�@��R@�~�@�$�@�{@�J@���@��@��@��@�@��@��u@�1@��@\)@~ȴ@}�T@}��@}��@}�h@}�@}?}@|�j@{�m@{C�@{@z�@z��@z=q@x��@x1'@w��@v�@u��@t��@t(�@s��@r~�@q��@q��@pĜ@pQ�@p1'@o�@ol�@n�R@nv�@n@mO�@l�D@k�@kS�@kC�@k33@j�@j��@j��@j��@j�\@j=q@i�@ix�@h�u@f��@e�-@d�/@dz�@dI�@c�
@ct�@b�H@a��@a�7@aG�@`�u@_�@_��@_l�@_K�@^v�@]��@]@]��@]�T@]�@]�T@]�T@]�-@]�@]�@\z�@\(�@\�@\�@[�
@[��@[�@[C�@[33@["�@Z��@Z^5@Y�#@Y�@X�u@XA�@Xb@W�;@W��@W�w@Wl�@WK�@WK�@W;d@W+@W�@W
=@V�y@V�@V5?@U�T@U��@UO�@T�/@Tz�@TZ@S�
@S"�@R��@RM�@R-@R-@Q��@Q��@Q&�@P�`@P�`@P��@P��@P1'@P �@P  @O��@Ol�@Nȴ@N��@Nv�@Nff@N5?@M�T@M?}@L�@LZ@K��@Kƨ@K"�@J��@J��@J~�@Jn�@J-@I��@I�7@IX@I7L@HĜ@H�@G�w@G
=@F5?@E�@D��@DI�@C�F@CS�@B�@B�\@B^5@B�@A��@Ax�@AX@@��@@bN@@b@?�@?�@?�;@?�@?�P@?�@?
=@>�@>$�@>{@=�@=/@<��@;��@;dZ@;S�@;"�@:~�@:J@9��@9%@8Ĝ@8�9@81'@7K�@6��@6@5��@5@5�-@5p�@5/@5�@5�@5V@4�@4(�@4�@3ƨ@3dZ@3C�@2��@2��@2^5@1�@1��@0Ĝ@0�@0bN@0b@/��@.��@.ȴ@.�R@.v�@.$�@-�@-��@-@-�@-O�@-/@,�@,�j@,��@,9X@,�@+�m@+��@+dZ@*�H@*^5@*J@)��@)��@)�@(��@(Ĝ@(bN@(A�@(A�@(1'@'�;@'l�@&�y@&{@%��@%�-@%�@$�@$I�@$�@#��@#�
@#�@#"�@"�!@"�!@"�!@"�!@"�!@"��@"~�@"^5@"�@!�#@!��@!��@!�7@!X@!�@!�@!%@ �`@ �9@ �u@ r�@ Q�@ 1'@�;@�P@;d@
=@�y@�R@��@ff@$�@�@�@?}@�@V@��@�@�D@�m@ƨ@�F@��@��@"�@�!@�@�@x�@7L@&�@�`@�9@r�@�@\)@;d@;d@�@�y@�R@v�@E�@{@�@��@p�@`B@O�@?}@?}@/@�@��@�/@��@�@j@j@j@I�@(�@1@�F@��@t�@dZ@dZ@C�@"�@@�@�H@��@�!@��@�\@�\@~�@M�@�@��@��@�@�^@�7@hs@7L@�@��@Ĝ@�9@��@�u@r�@b@��@�@��@|�@\)@\)@K�@\)@K�@K�@;d@�@�@��@��@�+@ff@5?@�@�-@�-@��@�h@�@�@`B@?}@?}@/@V@��@�@�/@��@�@j@Z@�@t�@�@t�@dZ@S�@S�@33@o@@
��@
��@
n�@
n�@
M�@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�M�A�O�A�S�A�ZA�ZA�XA�`BA�dZA�bNA�bNA�hsA�ffA�hsA�ffA�hsA�l�A�jA�n�A�l�A�n�A�r�A�n�A�r�A�n�A�t�A�v�A�z�A�v�A�z�A�|�A�~�AЁAЁAЁAЁAЁA�n�A� �A�bA�E�A�I�A�G�A�A�A��A��;A��+A���A���A�|�A�ffA��7A��
A�\)A��A���A�-A��A��A���A�Q�A��!A�hsAz$�AuO�Asx�AsAqC�Ao��AoAnn�AmXAkAh��Ae&�Ac"�Aa�FAaXA_\)A^1'A[ARVAK��AG�TAD�yA?�PA<=qA:��A: �A9��A8n�A6��A2(�A/oA.��A,��A,��A,I�A,5?A,bA*jA'�A&n�A$��A#��A$��A%`BA&r�A&�A&�jA&I�A&bA%��A%G�A%%A%oA%/A%S�A%\)A%G�A%G�A%;dA$�A$��A$��A$v�A$A"��A"�`A"ȴA"��A"Q�A!�FA!hsA ��A �+A �A�A7LA��A��A�A
=A��A�HA�\A1'A{A�^At�A+A��AJA\)A�`A�/A�Ar�A1'A�TAt�A��A�AVA�9A�uAE�A(�AJA�AƨA�hA?}A/A&�A�+A�A33A��An�A=qA9XA�A�#A�AC�A��A��AȴAn�AA�A{A�TA��AG�A�/A��Av�Ar�AbNA�+A�A�`A��A��A�+A^5A�TA33AĜAJA�
A�A�PAC�A�`Az�A$�A�7A?}A�A
�!A
�+A
  A	dZA��A��AE�A�;A�A�A^5A �A��AoA�RA�A^5A=qA5?A��A�A�yA�A��A7LA ��A M�@���@��+@�V@�E�@�5?@��-@�Q�@��@���@�X@�7L@���@�"�@�=q@�O�@���@���@�C�@�v�@�=q@��@�&�@��@��@���@�n�@��@�`B@���@�1@�@��@�5?@��T@�@陚@�7L@�Ĝ@� �@��m@�P@�+@��H@�R@�+@�V@�-@��@�V@���@�D@�@���@�n�@�5?@���@�p�@�&�@�Ĝ@��@ߥ�@ޏ\@݁@���@�{@١�@ف@�?}@���@�Ĝ@׮@ְ!@�$�@��#@պ^@�hs@���@�A�@�l�@��H@�5?@��T@љ�@�V@�Q�@ϥ�@�33@ΰ!@�-@�X@̼j@���@�"�@��H@���@ʗ�@ʏ\@�^5@�Ĝ@ǝ�@���@��@�7L@���@�1'@��m@ÍP@��y@�n�@��@�@��@��-@�hs@�V@��`@�z�@��@��P@�;d@���@���@�G�@��D@�A�@�l�@��+@�^5@��T@�O�@��`@�ƨ@���@�v�@��@�Ĝ@��;@�K�@���@�?}@���@��@��@�r�@�Z@�b@���@��P@�;d@�
=@��y@���@�V@�$�@��#@��-@��7@�`B@�O�@�?}@�7L@��@��@��/@�Ĝ@���@�A�@��
@�"�@���@���@��+@�~�@�J@��^@��7@�x�@��@��D@�Z@���@���@�t�@�K�@�o@���@�v�@��T@���@�@��h@�/@���@��@��@��@�"�@��+@�^5@�M�@���@��^@�hs@��@���@���@�r�@��m@�+@�ff@�$�@���@�x�@��@���@��@���@���@���@�V@��`@��j@�r�@�Q�@��@�|�@��@�ȴ@�E�@�O�@�Z@�ƨ@��y@�v�@��@���@��7@�V@��9@��@�Z@�I�@�1'@��@�K�@�v�@�$�@�J@��-@�hs@�`B@�`B@�X@�O�@�?}@��@��@���@���@�j@�Z@�I�@�1@���@�t�@�dZ@�K�@���@�V@�`B@�G�@��/@��9@�bN@�Q�@�A�@�(�@��@��@�+@��y@�ȴ@��!@��+@�V@�=q@�$�@�{@�J@���@���@���@�hs@��/@�Q�@�1@��m@��
@��w@���@�C�@��R@�~�@�$�@�{@�J@���@��@��@��@�@��@��u@�1@��@\)@~ȴ@}�T@}��@}��@}�h@}�@}?}@|�j@{�m@{C�@{@z�@z��@z=q@x��@x1'@w��@v�@u��@t��@t(�@s��@r~�@q��@q��@pĜ@pQ�@p1'@o�@ol�@n�R@nv�@n@mO�@l�D@k�@kS�@kC�@k33@j�@j��@j��@j��@j�\@j=q@i�@ix�@h�u@f��@e�-@d�/@dz�@dI�@c�
@ct�@b�H@a��@a�7@aG�@`�u@_�@_��@_l�@_K�@^v�@]��@]@]��@]�T@]�@]�T@]�T@]�-@]�@]�@\z�@\(�@\�@\�@[�
@[��@[�@[C�@[33@["�@Z��@Z^5@Y�#@Y�@X�u@XA�@Xb@W�;@W��@W�w@Wl�@WK�@WK�@W;d@W+@W�@W
=@V�y@V�@V5?@U�T@U��@UO�@T�/@Tz�@TZ@S�
@S"�@R��@RM�@R-@R-@Q��@Q��@Q&�@P�`@P�`@P��@P��@P1'@P �@P  @O��@Ol�@Nȴ@N��@Nv�@Nff@N5?@M�T@M?}@L�@LZ@K��@Kƨ@K"�@J��@J��@J~�@Jn�@J-@I��@I�7@IX@I7L@HĜ@H�@G�w@G
=@F5?@E�@D��@DI�@C�F@CS�@B�@B�\@B^5@B�@A��@Ax�@AX@@��@@bN@@b@?�@?�@?�;@?�@?�P@?�@?
=@>�@>$�@>{@=�@=/@<��@;��@;dZ@;S�@;"�@:~�@:J@9��@9%@8Ĝ@8�9@81'@7K�@6��@6@5��@5@5�-@5p�@5/@5�@5�@5V@4�@4(�@4�@3ƨ@3dZ@3C�@2��@2��@2^5@1�@1��@0Ĝ@0�@0bN@0b@/��@.��@.ȴ@.�R@.v�@.$�@-�@-��@-@-�@-O�@-/@,�@,�j@,��@,9X@,�@+�m@+��@+dZ@*�H@*^5@*J@)��@)��@)�@(��@(Ĝ@(bN@(A�@(A�@(1'@'�;@'l�@&�y@&{@%��@%�-@%�@$�@$I�@$�@#��@#�
@#�@#"�@"�!@"�!@"�!@"�!@"�!@"��@"~�@"^5@"�@!�#@!��@!��@!�7@!X@!�@!�@!%@ �`@ �9@ �u@ r�@ Q�@ 1'@�;@�P@;d@
=@�y@�R@��@ff@$�@�@�@?}@�@V@��@�@�D@�m@ƨ@�F@��@��@"�@�!@�@�@x�@7L@&�@�`@�9@r�@�@\)@;d@;d@�@�y@�R@v�@E�@{@�@��@p�@`B@O�@?}@?}@/@�@��@�/@��@�@j@j@j@I�@(�@1@�F@��@t�@dZ@dZ@C�@"�@@�@�H@��@�!@��@�\@�\@~�@M�@�@��@��@�@�^@�7@hs@7L@�@��@Ĝ@�9@��@�u@r�@b@��@�@��@|�@\)@\)@K�@\)@K�@K�@;d@�@�@��@��@�+@ff@5?@�@�-@�-@��@�h@�@�@`B@?}@?}@/@V@��@�@�/@��@�@j@Z@�@t�@�@t�@dZ@S�@S�@33@o@@
��@
��@
n�@
n�@
M�@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	gmB	gmB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	hsB	gmB	hsB	gmB	hsB	hsB	hsB	hsB	gmB	iyB	gmB	gmB	iyB	jB	iyB	iyB	hsB	hsB	hsB	hsB	iyB	iyB	iyB	l�B	t�B	�B	��B
�B
gmB
ȴB
��B
�sB
��BBoB�B%B
��B
��B
�mB
�9B
�hB
P�B
9XB
(�B
#�B
�B
bB
  B	�B	��B	ŢB	��B	�dB	�9B	�!B	�B	��B	��B	�{B	}�B	r�B	hsB	dZB	^5B	VB	J�B	.B	�B	�B	(�B	,B	%�B	!�B	 �B	"�B	"�B	�B	�B	DB		7B	
=B	�B	�B	!�B	33B	33B	)�B	(�B	'�B	-B	A�B	W
B	{�B	��B	��B	�wB	�B	�/B	�5B	�HB	�ZB	�mB	�B	��B
B
uB
�B
&�B
1'B
6FB
:^B
A�B
C�B
F�B
H�B
H�B
K�B
L�B
N�B
M�B
P�B
P�B
O�B
O�B
N�B
T�B
`BB
e`B
iyB
jB
iyB
hsB
gmB
iyB
iyB
hsB
e`B
`BB
[#B
YB
cTB
dZB
cTB
aHB
cTB
_;B
dZB
cTB
bNB
aHB
bNB
e`B
ffB
gmB
hsB
hsB
hsB
hsB
jB
m�B
m�B
ffB
gmB
dZB
dZB
e`B
hsB
hsB
iyB
iyB
iyB
jB
iyB
m�B
m�B
m�B
l�B
k�B
iyB
gmB
e`B
dZB
cTB
cTB
cTB
ffB
p�B
s�B
s�B
s�B
r�B
q�B
n�B
iyB
e`B
`BB
^5B
]/B
]/B
_;B
`BB
_;B
]/B
ZB
W
B
YB
W
B
VB
R�B
P�B
M�B
K�B
K�B
J�B
H�B
F�B
E�B
D�B
C�B
A�B
@�B
?}B
>wB
>wB
>wB
?}B
=qB
A�B
@�B
>wB
>wB
>wB
?}B
=qB
<jB
<jB
<jB
;dB
:^B
:^B
8RB
8RB
7LB
6FB
6FB
6FB
49B
33B
2-B
1'B
0!B
0!B
/B
/B
.B
/B
-B
-B
-B
,B
,B
+B
+B
)�B
)�B
(�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
#�B
"�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
L�B
K�B
K�B
K�B
K�B
M�B
M�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
XB
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
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
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
`BB
`BB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	gmB	gmB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	gmB	hsB	hsB	gmB	hsB	gmB	hsB	hsB	hsB	hsB	gmB	iyB	gmB	gmB	iyB	jB	iyB	iyB	hsB	hsB	hsB	hsB	iyB	iyB	iyB	l�B	t�B	�B	��B
�B
gmB
ȴB
��B
�sB
��BBoB�B%B
��B
��B
�mB
�9B
�hB
P�B
9XB
(�B
#�B
�B
bB
  B	�B	��B	ŢB	��B	�dB	�9B	�!B	�B	��B	��B	�{B	}�B	r�B	hsB	dZB	^5B	VB	J�B	.B	�B	�B	(�B	,B	%�B	!�B	 �B	"�B	"�B	�B	�B	DB		7B	
=B	�B	�B	!�B	33B	33B	)�B	(�B	'�B	-B	A�B	W
B	{�B	��B	��B	�wB	�B	�/B	�5B	�HB	�ZB	�mB	�B	��B
B
uB
�B
&�B
1'B
6FB
:^B
A�B
C�B
F�B
H�B
H�B
K�B
L�B
N�B
M�B
P�B
P�B
O�B
O�B
N�B
T�B
`BB
e`B
iyB
jB
iyB
hsB
gmB
iyB
iyB
hsB
e`B
`BB
[#B
YB
cTB
dZB
cTB
aHB
cTB
_;B
dZB
cTB
bNB
aHB
bNB
e`B
ffB
gmB
hsB
hsB
hsB
hsB
jB
m�B
m�B
ffB
gmB
dZB
dZB
e`B
hsB
hsB
iyB
iyB
iyB
jB
iyB
m�B
m�B
m�B
l�B
k�B
iyB
gmB
e`B
dZB
cTB
cTB
cTB
ffB
p�B
s�B
s�B
s�B
r�B
q�B
n�B
iyB
e`B
`BB
^5B
]/B
]/B
_;B
`BB
_;B
]/B
ZB
W
B
YB
W
B
VB
R�B
P�B
M�B
K�B
K�B
J�B
H�B
F�B
E�B
D�B
C�B
A�B
@�B
?}B
>wB
>wB
>wB
?}B
=qB
A�B
@�B
>wB
>wB
>wB
?}B
=qB
<jB
<jB
<jB
;dB
:^B
:^B
8RB
8RB
7LB
6FB
6FB
6FB
49B
33B
2-B
1'B
0!B
0!B
/B
/B
.B
/B
-B
-B
-B
,B
,B
+B
+B
)�B
)�B
(�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
#�B
"�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
"�B
"�B
"�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
L�B
K�B
K�B
K�B
K�B
M�B
M�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
VB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
XB
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
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
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
`BB
`BB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210306080046                              AO  ARCAADJP                                                                    20210306080046    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210306080046  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210306080046  QCF$                G�O�G�O�G�O�0               