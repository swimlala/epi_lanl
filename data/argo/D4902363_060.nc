CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-23T00:35:24Z creation;2016-11-23T00:35:26Z conversion to V3.1;2019-12-19T08:24:50Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161123003524  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA  I2_0576_060                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5� <�1   @��6�8�@:��4֡b�d�\����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   AffA>ffA`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�fD�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��A��A<��A^�\A�{A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B@
=BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD� RD�@RD�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��RD��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AͮAͮAͬAͰ!AͰ!Aͩ�AͰ!AͶFAͶFA͸RA͸RAͺ^Aͺ^Aͺ^A;wA;wA�A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A�ȴA�AͲ-A�v�A�l�A�A� �AĴ9A���A��-A�ZA��HA�-A��HA�l�A��A�9XA�G�A�ȴA���A�z�A���A���A�/A���A��A���A���A���A�O�A���A�O�A���A�bA�z�A�z�A��yA��TA��A���A�ĜA�+A��
A���A�z�A�G�A�^5A�l�A��A�n�A��`A�n�A���A�?}A��7A�1A�A�A� �A��A�9XA��A��A�E�A���A��A��-A��A���A�+A�ZA��\A��yA��`A���A�l�A�1'A���A�7LA��A���A�O�A���A�VA��jA��A~�A}K�A|5?A{�hA{Ay��AwG�AvZAu�At��As��Ar�yArE�AodZAl�`Ak/Ah�!Ag/AfffAeAdZAbA_hsA^VA\�!A[hsAZ��AYK�AX�RAW��AV�AUO�AT�!AT=qASoAPbAMx�AL�AL�AJ�AJ1'AI�AHffAF�uAF-AF5?AD�`AChsABr�A@�HA@I�A?��A?�FA@{A?A>ffA<ffA;�PA:��A9��A8Q�A7l�A6�/A6ffA6bA4��A3��A2��A1�mA1�A1x�A/;dA.�jA.�\A.bA,�+A*$�A)A)��A(�9A'G�A&I�A%|�A%33A#�A#��A"��A"1'A!`BA �A 9XA�PA%A(�A�\Ap�A��A��AoA�HAffAƨA��A$�A�7A%AVA7LAn�A��AhsA��A~�A�A��AC�A^5A�wA
��A
~�A
bA	�A�jAjAE�A �A�mA`BAȴA5?A��A�^A��A��A��A�FA"�A �j@�l�@��@���@��y@��@�`B@��`@� �@�l�@�V@��u@�@�M�@�hs@�9X@�`B@�?}@�1'@�\)@��#@��;@��@��`@�(�@��@�\)@ޟ�@�J@�`B@�Q�@ڗ�@��@ٙ�@أ�@ׅ@֏\@�G�@���@�$�@��H@�E�@�@͑h@�/@�A�@���@�`B@��`@ț�@�9X@Ǖ�@���@�`B@���@�t�@���@���@��+@���@�%@���@�=q@�j@��w@�dZ@�hs@�Q�@�(�@��@�;d@�=q@�X@��@��D@��@�\)@�5?@��#@���@�&�@���@���@��
@�J@��@�A�@��w@��@�n�@���@�Z@�|�@��+@�V@�p�@�A�@�1@�dZ@�^5@���@�`B@��@�j@�b@��@���@�M�@�@���@��@�Ĝ@��D@���@�t�@�o@���@��H@���@��+@��\@�ff@�V@�-@�@��@��@���@��@��@��@�K�@�o@�E�@�J@�p�@�/@��9@�9X@���@�\)@��@�
=@���@���@�ff@�{@���@�7L@�Ĝ@��@�A�@�b@�  @��;@�dZ@���@�E�@�5?@�=q@�=q@��@��`@���@��D@�r�@�j@�A�@�1'@� �@��@�  @��;@��@���@���@��R@��\@�n�@�E�@��@��@���@��@�?}@���@���@��9@���@��@�b@��m@��@�dZ@�+@���@��y@��R@��\@�v�@�E�@���@���@�p�@���@��`@��/@��j@�r�@�Z@��@��@;d@~��@~��@~�+@~@}?}@}V@|�j@|��@|I�@{��@z�H@z�\@z=q@y��@y��@yx�@y7L@x��@x��@x�u@xbN@x �@w�w@wl�@wK�@vE�@uO�@t�/@tz�@s�m@s�@s�@sS�@r~�@r�@q�7@q&�@q�@p��@p1'@o�;@o�;@p  @p1'@o��@nȴ@n��@n�+@n��@n��@o+@o+@nȴ@m@l�@lI�@k��@k��@l1@k�
@kC�@j�H@j^5@jJ@i�@i��@i�^@i�^@ix�@h��@hbN@g��@g�P@g
=@f�R@fv�@fE�@f{@fff@fV@fV@fE�@f5?@e�T@e�@d�@d�@d�/@c��@cC�@b�@b�H@b��@bn�@bn�@bM�@a�7@a�@a%@`�`@`�9@`�u@`b@_�;@_�@_;d@^��@^�@^��@_;d@^��@^�@^v�@]��@]p�@\�@\�D@\(�@[��@Z��@Z��@Z��@Z��@Z�@Y��@YX@Y�@X�`@X�9@X�@XbN@XQ�@W�@W�w@W;d@W
=@Vȴ@V5?@V{@U�@U@U�-@U?}@T��@T�j@T�@T��@T�@S��@S@R^5@Q�@Q��@QG�@Q&�@Q�@P��@P�u@Pr�@PQ�@P �@O�;@O��@OK�@O+@O+@N��@N��@N�+@M�h@L��@L��@L�@L�D@L9X@L(�@K�m@K�@K"�@J��@J^5@JJ@H�`@H �@G�@G�P@F�@Fv�@F$�@F@E�@E��@E�-@EV@D�j@D��@DI�@D�@D1@C��@Cƨ@Ct�@C"�@B^5@A�#@A�^@A%@@��@@�9@@Q�@@  @?�@?�;@?�@?\)@?�@>ȴ@>�+@>V@>$�@=�T@=`B@=�@<��@<��@<j@<1@;�@;S�@;C�@;33@;o@:��@:^5@:=q@9�@9��@9hs@9&�@8��@8�`@8��@8�@8A�@8  @7�;@7�@6��@6ȴ@6ff@65?@5�T@5�-@5O�@4��@4�D@49X@3�m@3t�@3o@2�@2�@2�@2�@2�H@2�H@2�@2�@3@2�@3@2�@2�@2�H@2��@2��@2��@2��@2^5@2-@1��@1��@1&�@0��@0��@0Q�@/�@/;d@.�y@.�R@.��@.ff@.E�@.$�@.{@.{@-�@-@-p�@-?}@,�@,�D@,j@,�@+�
@+�F@+��@+dZ@+33@+o@*��@*M�@*J@)��@)�^@)�^@)�7@)hs@)hs@)hs@)X@)X@)X@)7L@)�@(��@(1'@'�@'�@'�w@'l�@';d@&ȴ@&��@&v�@&ff@&V@&V@&{@%�T@%O�@$�j@$�D@$j@$I�@$9X@$�@#�m@#ƨ@#t�@#S�@#@"�!@"-@"J@!�#@!��@!��@!�7@!G�@!&�@!�@ ��@ ��@ 1'@�@��@�@�P@\)@
=@��@V@$�@�-@�@V@�/@��@�j@z�@ƨ@t�@"�@��@�!@�!@��@n�@^5@=q@=q@-@�#@x�@hs@7L@�`@�9@�@bN@b@�;@�w@�@|�@;d@ȴ@��@v�@5?@@��@��@@O�@��@��@�D@Z@9X@(�@(�@�@��@�
@��@�@dZ@33@"�@o@@�!@-@�^@�7@X@7L@��@��@�@A�@b@�;@l�@;d@�y@�+@V@$�@�@�T@��@�-@?}@�j@��@Z@�@1@�m@��@dZ@S�@@
=q@	�#@	��@	��@	�^@	�^@	��@	��@	��@	��@	�7@	hs@	7L@	%@�`@�`@�@r�@1'@�;@��@�w@�@�w@�@��@��@��@�@�@�@�@|�@�@V@$�@@�-@�h@V@��@��@��@z�@z�@j@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AͮAͮAͬAͰ!AͰ!Aͩ�AͰ!AͶFAͶFA͸RA͸RAͺ^Aͺ^Aͺ^A;wA;wA�A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A�ȴA�AͲ-A�v�A�l�A�A� �AĴ9A���A��-A�ZA��HA�-A��HA�l�A��A�9XA�G�A�ȴA���A�z�A���A���A�/A���A��A���A���A���A�O�A���A�O�A���A�bA�z�A�z�A��yA��TA��A���A�ĜA�+A��
A���A�z�A�G�A�^5A�l�A��A�n�A��`A�n�A���A�?}A��7A�1A�A�A� �A��A�9XA��A��A�E�A���A��A��-A��A���A�+A�ZA��\A��yA��`A���A�l�A�1'A���A�7LA��A���A�O�A���A�VA��jA��A~�A}K�A|5?A{�hA{Ay��AwG�AvZAu�At��As��Ar�yArE�AodZAl�`Ak/Ah�!Ag/AfffAeAdZAbA_hsA^VA\�!A[hsAZ��AYK�AX�RAW��AV�AUO�AT�!AT=qASoAPbAMx�AL�AL�AJ�AJ1'AI�AHffAF�uAF-AF5?AD�`AChsABr�A@�HA@I�A?��A?�FA@{A?A>ffA<ffA;�PA:��A9��A8Q�A7l�A6�/A6ffA6bA4��A3��A2��A1�mA1�A1x�A/;dA.�jA.�\A.bA,�+A*$�A)A)��A(�9A'G�A&I�A%|�A%33A#�A#��A"��A"1'A!`BA �A 9XA�PA%A(�A�\Ap�A��A��AoA�HAffAƨA��A$�A�7A%AVA7LAn�A��AhsA��A~�A�A��AC�A^5A�wA
��A
~�A
bA	�A�jAjAE�A �A�mA`BAȴA5?A��A�^A��A��A��A�FA"�A �j@�l�@��@���@��y@��@�`B@��`@� �@�l�@�V@��u@�@�M�@�hs@�9X@�`B@�?}@�1'@�\)@��#@��;@��@��`@�(�@��@�\)@ޟ�@�J@�`B@�Q�@ڗ�@��@ٙ�@أ�@ׅ@֏\@�G�@���@�$�@��H@�E�@�@͑h@�/@�A�@���@�`B@��`@ț�@�9X@Ǖ�@���@�`B@���@�t�@���@���@��+@���@�%@���@�=q@�j@��w@�dZ@�hs@�Q�@�(�@��@�;d@�=q@�X@��@��D@��@�\)@�5?@��#@���@�&�@���@���@��
@�J@��@�A�@��w@��@�n�@���@�Z@�|�@��+@�V@�p�@�A�@�1@�dZ@�^5@���@�`B@��@�j@�b@��@���@�M�@�@���@��@�Ĝ@��D@���@�t�@�o@���@��H@���@��+@��\@�ff@�V@�-@�@��@��@���@��@��@��@�K�@�o@�E�@�J@�p�@�/@��9@�9X@���@�\)@��@�
=@���@���@�ff@�{@���@�7L@�Ĝ@��@�A�@�b@�  @��;@�dZ@���@�E�@�5?@�=q@�=q@��@��`@���@��D@�r�@�j@�A�@�1'@� �@��@�  @��;@��@���@���@��R@��\@�n�@�E�@��@��@���@��@�?}@���@���@��9@���@��@�b@��m@��@�dZ@�+@���@��y@��R@��\@�v�@�E�@���@���@�p�@���@��`@��/@��j@�r�@�Z@��@��@;d@~��@~��@~�+@~@}?}@}V@|�j@|��@|I�@{��@z�H@z�\@z=q@y��@y��@yx�@y7L@x��@x��@x�u@xbN@x �@w�w@wl�@wK�@vE�@uO�@t�/@tz�@s�m@s�@s�@sS�@r~�@r�@q�7@q&�@q�@p��@p1'@o�;@o�;@p  @p1'@o��@nȴ@n��@n�+@n��@n��@o+@o+@nȴ@m@l�@lI�@k��@k��@l1@k�
@kC�@j�H@j^5@jJ@i�@i��@i�^@i�^@ix�@h��@hbN@g��@g�P@g
=@f�R@fv�@fE�@f{@fff@fV@fV@fE�@f5?@e�T@e�@d�@d�@d�/@c��@cC�@b�@b�H@b��@bn�@bn�@bM�@a�7@a�@a%@`�`@`�9@`�u@`b@_�;@_�@_;d@^��@^�@^��@_;d@^��@^�@^v�@]��@]p�@\�@\�D@\(�@[��@Z��@Z��@Z��@Z��@Z�@Y��@YX@Y�@X�`@X�9@X�@XbN@XQ�@W�@W�w@W;d@W
=@Vȴ@V5?@V{@U�@U@U�-@U?}@T��@T�j@T�@T��@T�@S��@S@R^5@Q�@Q��@QG�@Q&�@Q�@P��@P�u@Pr�@PQ�@P �@O�;@O��@OK�@O+@O+@N��@N��@N�+@M�h@L��@L��@L�@L�D@L9X@L(�@K�m@K�@K"�@J��@J^5@JJ@H�`@H �@G�@G�P@F�@Fv�@F$�@F@E�@E��@E�-@EV@D�j@D��@DI�@D�@D1@C��@Cƨ@Ct�@C"�@B^5@A�#@A�^@A%@@��@@�9@@Q�@@  @?�@?�;@?�@?\)@?�@>ȴ@>�+@>V@>$�@=�T@=`B@=�@<��@<��@<j@<1@;�@;S�@;C�@;33@;o@:��@:^5@:=q@9�@9��@9hs@9&�@8��@8�`@8��@8�@8A�@8  @7�;@7�@6��@6ȴ@6ff@65?@5�T@5�-@5O�@4��@4�D@49X@3�m@3t�@3o@2�@2�@2�@2�@2�H@2�H@2�@2�@3@2�@3@2�@2�@2�H@2��@2��@2��@2��@2^5@2-@1��@1��@1&�@0��@0��@0Q�@/�@/;d@.�y@.�R@.��@.ff@.E�@.$�@.{@.{@-�@-@-p�@-?}@,�@,�D@,j@,�@+�
@+�F@+��@+dZ@+33@+o@*��@*M�@*J@)��@)�^@)�^@)�7@)hs@)hs@)hs@)X@)X@)X@)7L@)�@(��@(1'@'�@'�@'�w@'l�@';d@&ȴ@&��@&v�@&ff@&V@&V@&{@%�T@%O�@$�j@$�D@$j@$I�@$9X@$�@#�m@#ƨ@#t�@#S�@#@"�!@"-@"J@!�#@!��@!��@!�7@!G�@!&�@!�@ ��@ ��@ 1'@�@��@�@�P@\)@
=@��@V@$�@�-@�@V@�/@��@�j@z�@ƨ@t�@"�@��@�!@�!@��@n�@^5@=q@=q@-@�#@x�@hs@7L@�`@�9@�@bN@b@�;@�w@�@|�@;d@ȴ@��@v�@5?@@��@��@@O�@��@��@�D@Z@9X@(�@(�@�@��@�
@��@�@dZ@33@"�@o@@�!@-@�^@�7@X@7L@��@��@�@A�@b@�;@l�@;d@�y@�+@V@$�@�@�T@��@�-@?}@�j@��@Z@�@1@�m@��@dZ@S�@@
=q@	�#@	��@	��@	�^@	�^@	��@	��@	��@	��@	�7@	hs@	7L@	%@�`@�`@�@r�@1'@�;@��@�w@�@�w@�@��@��@��@�@�@�@�@|�@�@V@$�@@�-@�h@V@��@��@��@z�@z�@j@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBɺBɺBɺBɺBɺBɺB��B�5B�B�B�B��B��B��B�B��B��B��B�yB�TB�/B�#B�B��B�dB�?B�B��B��B��B�Bq�Bp�Bu�Bq�B_;B �BhB+B�B�B�yB�fB�`B��BȴBŢB��B�NB�B��B��B��B+BB  B��B�B�fB��B�B��B��B�oB�JB�+B�Bw�Bm�BdZBZBK�BB�B7LB6FB6FB)�BoB+BB
��B
�sB
�;B
�#B
ȴB
�wB
�!B
��B
�PB
�B
|�B
v�B
n�B
\)B
P�B
G�B
=qB
5?B
+B
#�B
1B	�B	�/B	ĜB	�?B	�B	��B	��B	�\B	|�B	y�B	x�B	o�B	m�B	e`B	_;B	ZB	R�B	I�B	E�B	E�B	@�B	1'B	#�B	�B	�B	�B	bB	\B	VB	B	B	%B	DB	  B��B�fB�`B�ZB�yB��B��B��B�B�B�sB�`B�)B��B��B��B��BɺB��B�XB�?B�FB�FB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�hB�\B�PB�DB�+B�B� B|�By�Bx�Bv�Bx�Bs�Bp�Bp�Bn�Bm�BjBgmBdZBbNBaHB[#BYBW
BW
BT�BQ�BP�BP�BO�BO�BN�BM�BL�BL�BK�BK�BI�BG�BE�BE�BD�B@�B>wB<jB:^B9XB8RB5?B33B33B2-B1'B1'B1'B0!B0!B0!B.B-B,B(�B$�B�B�B�B�B�B�B�B�B{B{BuBuBoBuBuBoBoBuB{B{B�B�B�B�B�B�B�B�B�B�B#�B%�B%�B%�B'�B%�B'�B(�B(�B&�B"�B"�B#�B&�B)�B(�B)�B)�B2-B/B.B0!B1'B6FB:^B;dB;dB;dB:^B;dB<jB=qB>wBA�BC�BD�BF�BG�BG�BG�BI�BI�BM�BO�BS�BS�BT�BT�BW
BVBVBXB]/BaHBbNBe`BgmBhsBjBo�Bq�Br�Bs�Bt�Bv�Bw�Bz�B|�B� B�B�B�B�B�B�%B�%B�1B�1B�JB�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�FB�dB�}BÖBƨBǮBɺB��B��B��B��B��B��B��B�B�
B�B�`B�mB�sB�sB�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	%B	1B		7B	
=B	JB	PB	\B	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	$�B	'�B	)�B	.B	2-B	2-B	2-B	49B	8RB	:^B	;dB	>wB	A�B	B�B	D�B	H�B	J�B	M�B	O�B	T�B	XB	\)B	_;B	iyB	k�B	m�B	o�B	p�B	t�B	w�B	x�B	y�B	z�B	{�B	}�B	~�B	� B	�B	�B	�=B	�JB	�PB	�\B	�hB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�9B	�9B	�FB	�XB	�^B	�dB	�dB	�jB	�qB	�}B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�HB	�NB	�TB	�TB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
VB
\B
\B
bB
bB
hB
hB
uB
uB
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
-B
,B
-B
-B
-B
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
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
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
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
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
VB
VB
W
B
VB
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
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
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BȴBȴBȴBȴBȴBȴBȴB��BȴBȴBȴBȴBȴBȴB��BȴBȴBȴB��BȴBȴBȴBȴBȴBȴB��B��BȴBȴBȴBȴBȴB��BɺB��B��B�XB�B�BԯB�,B�B�-B�B�$B��B��B�`B��B��B��B�kB�ZB�B�B�WB�,B�wB��B�]B�sB�NB��B�7BsMBrBw�Bv�BeFB#nB�B	B��B��B�KB�$B�BЗB�B�B͹B�B�B�?B�>B�6B�BSB�B�dB�B��B��B�B�vB��B��B�6B��B��By>Bn�Be�B\BM�BC�B7�B7�B9	B,qB�BKB�B
��B
��B
�HB
��B
ʦB
� B
��B
�]B
��B
�B
~(B
x�B
p�B
]dB
RB
H�B
>�B
6zB
,�B
'B
)B	��B	��B	ƎB	�`B	�iB	�B	��B	� B	~�B	{�B	zDB	p�B	oiB	ffB	`�B	[�B	S�B	J�B	F�B	G�B	C�B	3�B	$�B	�B	]B	�B	hB	NB	B	�B	�B	�B	B	oB�rB�RB��B�B�B��B��B�B��B�B��B�8B�dB��BңBѷB�NB�^B�B�^B��B�fB��B��B��B�)B�=B�eB��B��B�kB��B�8B��B��B�-B�]B��B��B��B�@B�oB��B�}B��B�PB��B�GB�oB}�BzxBy�BxBz*Bt�Bq�Bq�Bo�BoBk�Bh�BezBdZBb�B[�BY�BW�BXEBU�BR�BQ�BQ�BP�BP�BOvBN"BM6BMjBL�BL�BJ�BHKBF?BF�BFtBBB?�B=qB;JB:�B:B6B3�B3�B2�B1�B1�B1�B1'B1[B1AB.�B-�B-CB+6B'8B�BKB�B�B�B1BB�B�B�BB,B@B{B�B�B@BFBMB�B�BBxB)B#B�B)B~B�B �B$ZB&2B&fB&�B)B&�B(�B*eB+B'�B#:B#�B$�B(>B*�B*0B*�B*�B3hB/�B.cB0�B1�B7B:�B;�B;�B;�B;0B<6B<�B=�B>�BA�BC�BEmBG�BHfBHfBH1BJ=BJrBN�BP�BT�BT�BU�BU�BW�BVmBV�BX�B]�Ba�Bb�Be�Bg�Bh�BkBp!BrBr�BtBu?BwBxlB{JB}VB�4B�[B�aB�-B�GB�gB�YB��B��B��B��B��B��B��B��B��B�B�]B�B�-B�:B�ZB�eB��B�CB�CB�/B�iB��B��B��B��B� B��B��B��B��B��B��B�0B�JB�6B� B�B�B�YB�B�zB�B�B�B�B�B�B�B�B��B�/B�B��B��B�B�"B�(B	 OB	UB	uB	SB	tB	fB		�B	
rB	dB	�B	�B	�B	�B	�B	�B	�B	�B	 B	"B	#B	# B	%,B	($B	*eB	.}B	2aB	2GB	2|B	4nB	8lB	:�B	;�B	>�B	A�B	B�B	D�B	H�B	KB	NB	P.B	U2B	XEB	\xB	_�B	i�B	k�B	m�B	o�B	p�B	t�B	xB	y	B	zB	z�B	|6B	~BB	.B	�OB	��B	��B	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�DB	�kB	�kB	�0B	�0B	��B	�0B	�6B	�=B	�]B	�iB	�AB	�GB	�GB	�hB	�MB	�TB	��B	�zB	��B	��B	��B	��B	��B	��B	�}B	��B	B	ÖB	��B	ðB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�.B	�,B	�B	�B	�?B	�+B	�EB	�1B	�KB	�WB	�]B	�CB	�/B	�OB	�|B	�hB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�"B	�(B	�.B	�HB
 B
 �B
aB
mB
SB
?B
EB
EB
EB
EB
fB
fB
fB
	RB
	RB
	lB

XB

XB

rB
^B
~B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
!B
!�B
!�B
#B
#B
#�B
#�B
$�B
$�B
&B
'B
(
B
($B
($B
)B
)*B
*0B
*B
*B
*0B
+6B
+QB
,"B
-)B
,"B
-)B
-CB
-CB
./B
./B
.IB
/OB
/OB
/OB
0!B
0UB
0;B
0UB
1[B
1[B
1[B
1vB
2GB
2GB
2aB
3MB
3hB
3�B
4�B
4TB
5tB
5ZB
5�B
6`B
6zB
7fB
7LB
7fB
7fB
7LB
7fB
7fB
7LB
7LB
7fB
7fB
7LB
7fB
7LB
7fB
7LB
7�B
8�B
8�B
8lB
8�B
9�B
:xB
:�B
:�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=qB
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
IB
J	B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
MB
L�B
MB
M�B
M�B
M�B
NB
OB
N�B
OB
O�B
O�B
PB
PB
QB
QB
QB
Q B
RB
RB
R:B
S&B
SB
S&B
T,B
TFB
UB
UB
U2B
UB
U2B
UB
UB
U2B
UB
UB
UB
V9B
VB
VB
VB
W$B
V9B
W$B
X+B
XEB
XEB
YKB
YKB
YKB
ZQB
Z7B
[=B
[WB
[=B
[=B
[qB
\]B
\]B
\]B
]IB
]IB
]/B
]IB
]~B
]dB
^jB
^jB
_VB
_VB
_VB
_;B
_;B
_pB
_VB
`\B
`\B
`\B
`vB
`\B
`\B
`vB
`�B
a�B
bhB
bhB
bhB
c�B
cnB
cnB
cnB
d�B
dtB
dtB
ezB
ezB
ezB
f�B
f�B
f�B
f�B
ffB
g�B
g�B
f�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611270036232016112700362320161127003623201806221217142018062212171420180622121714201804050410142018040504101420180405041014  JA  ARFMdecpA19c                                                                20161123093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161123003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161123003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161123003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161123003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161123003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161123003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161123003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161123003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161123003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20161123013044                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161123153312  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161126153623  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161126153623  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191014  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                