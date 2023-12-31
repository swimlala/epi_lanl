CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-15T00:35:13Z creation;2017-12-15T00:35:17Z conversion to V3.1;2019-12-19T07:54:16Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171215003513  20200115121519  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_189                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�<�c�9 1   @�<�q� @;�_o� �dYJ#9��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�fD�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @:=q@z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��RD��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D���D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�@RD�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��RD��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��RD��D�#�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��+A��+A��+A��+A��7A��DA��DA��DA��PA��PA��\A��\A��hA��hA��uA��DA��A�hsA��A��A���A�$�A�XA�l�A��A��A��A���A��\A�7LA��-A���A��A�+A�/A�+A��TA�p�A�;dA��A��FA�-A��-A�dZA���A�C�A��;A�t�A��A�XA���A���A��+A�ZA���A���A�`BA�ffA���A�r�A�-A�Q�A�O�A��A�A���A��9A�A�A~5?A}A|^5Azz�Ay
=Axr�Aw�Au�PAs�ArM�ApZAo"�An�An��AnI�Al�Ak��Aj��Aj�uAjbNAjE�Aj�Ai�^AihsAh�Ag\)Ae��AdbAbQ�AaƨA`I�A^�A]l�A\ffA\�AZZAX�9AW�hAVjAUK�ATn�AS��AS/ARVAQ��AQ33AP��AP1AO�AN�AMoALM�AKƨAJ��AIK�AGhsAE��AD��ADJAC��AC7LAB��ABI�AA�AAhsAAK�AAVA@I�A?G�A=G�A;�
A:�+A:VA9�A8$�A7��A7�A6�!A69XA4��A3�;A3C�A2v�A1`BA0��A0��A0^5A0 �A/�TA/��A/A.$�A-S�A,�+A,A�A,$�A, �A, �A, �A,�A+�#A+;dA+�A*��A)�A(��A(1A'�hA'"�A&ȴA%��A%+A$Q�A#p�A"M�A!�A!+A�;Ap�AK�A��A��A�+AM�AJA�A;dA9XA�A��A�uAJA�AK�A%A��AO�A �A�RA+A~�A�7A+A�+AO�A$�A
ffA��AjA�A  A��A�A/AVAG�A�TA��A�+A�TA�^AS�A  �@���A {@���@�v�@���@��@�/@�Q�@��@�{@���@��@�r�@�E�@��@��@�hs@�?}@��@���@�|�@���@�~�@�p�@�@�ff@�bN@�@��@�@ߍP@�{@��/@���@���@��#@�X@���@��@�1'@���@�V@�l�@�$�@���@�O�@̋D@��H@��/@���@�K�@��@�"�@��#@��@�9X@�@�@�?}@�(�@�@���@�&�@�K�@�V@��#@���@�O�@�V@�1'@��P@���@�v�@�x�@�j@�A�@�"�@��9@�b@�\)@��@�@�`B@��`@��9@���@��u@��@�z�@�z�@�z�@�z�@�bN@�A�@� �@���@���@�33@��H@���@�V@�X@��9@�Q�@�t�@�{@��h@�X@�O�@�7L@�V@��@�\)@��H@�ff@�x�@��`@�j@�S�@���@�V@��@���@��/@�z�@��;@�C�@�~�@���@��@�r�@��m@��P@�C�@���@���@�V@���@���@��^@�x�@�&�@�%@��/@�z�@�Q�@�b@���@�|�@�C�@�
=@��+@���@�&�@��@���@�"�@�o@���@��T@���@�hs@�%@��9@�A�@��m@��P@���@�-@��#@�p�@��@�V@���@�Z@��@��F@���@��@�l�@�\)@�K�@�C�@�33@�"�@���@�ȴ@��!@��+@�^5@���@��@�%@��D@�1'@�1@�@��@�@|�@K�@�@~�y@~�@~��@~E�@~$�@}�@}��@}�@}p�@}O�@}?}@}/@|��@|9X@{�@z�H@z��@z�\@z^5@yx�@x �@w�P@wl�@w�@v�@v�R@v��@v��@w
=@w�@v�y@v�R@vv�@v$�@u��@uO�@tz�@s�m@so@r��@r��@r~�@r^5@rM�@r=q@r-@q��@qhs@p��@pĜ@p�u@pr�@p1'@pb@o�;@o�w@o��@n��@nȴ@n�R@n�R@n��@n��@n��@n�+@nE�@m�T@m��@o
=@o|�@o+@nV@l�@k@jn�@j-@j-@jJ@ix�@h�`@gK�@g
=@g\)@h  @i&�@i%@hbN@g�@g;d@fȴ@f�+@fV@fE�@f$�@e�@e��@e@e��@e��@e`B@eO�@d�@d9X@cS�@b�H@b�@b��@bM�@b=q@a�^@aG�@a7L@`�9@` �@_�;@_�P@_\)@_\)@_�P@_\)@]��@]O�@]O�@\��@\�D@[ƨ@[S�@[o@[@Z��@Z�\@Z~�@ZM�@Z=q@Z-@ZJ@Y��@Y%@X�9@Xr�@XA�@Xb@W�;@W�;@W�;@W�@W�;@W|�@V��@V5?@Up�@UO�@UV@T�j@T�D@Tj@TI�@T(�@S�m@St�@S33@So@R��@R�@Q�^@Q��@Q��@QX@Q�@P�`@PĜ@P1'@O��@Ol�@O+@N��@N��@Nv�@NE�@N$�@M��@M�-@Mp�@L��@L��@L�D@K"�@JM�@J-@J�@I�#@I�7@H��@G�;@G�w@G��@GK�@G;d@G;d@G�@G+@G+@G
=@G
=@F�y@F�@F5?@E�-@E�h@Ep�@E`B@E?}@EV@D�D@D�@C��@B�\@BM�@B-@A�@A��@Ahs@AG�@A�@@�9@@bN@@b@?�@?l�@>�y@>��@>V@>E�@>{@=�@=�@=?}@=/@=�@=V@<�@<��@<�j@;��@;C�@:n�@:�@9��@9G�@9�@8��@8��@8��@8�@8bN@8bN@8bN@8b@7l�@7+@6��@6��@6�+@6v�@6ff@6V@6$�@6@5@5`B@5�@4�/@4I�@4(�@3�m@3��@2�H@2n�@1��@1��@1X@0�`@0Ĝ@0�@01'@/��@/��@/;d@/
=@.ȴ@.��@.{@-@-p�@,�@,�@,z�@,I�@,�@+��@+�
@+ƨ@+��@+t�@+dZ@+"�@*�H@*��@*��@*~�@*^5@*=q@)�@)�@)��@)��@)��@)��@)x�@)hs@)X@)�@(�`@(��@(�9@(�@(A�@(1'@'�@'l�@'K�@';d@'+@'+@&ȴ@&��@&v�@&ff@&V@&V@&E�@&5?@%�@%�-@%�h@%�@%?}@%�@$�/@$��@$z�@$Z@$(�@#�
@#�F@#��@#��@#dZ@#33@#@"�\@"^5@"-@!�^@!7L@ ��@ ��@ �u@ bN@ 1'@�;@��@;d@�R@��@v�@E�@�T@`B@�@��@�D@z�@Z@9X@�@�
@t�@33@@�\@^5@�@�^@�7@x�@�@ �@�@+@�@��@v�@V@$�@�T@@�@O�@�@�D@Z@�@��@�
@dZ@o@@�@��@=q@hs@7L@�@��@�`@��@��@Ĝ@�9@��@r�@�;@ȴ@�R@��@E�@$�@{@��@�h@?}@?}@p�@p�@`B@O�@�@��@��@9X@�
@��@��@�@�@t�@t�@dZ@S�@S�@S�@33@"�@"�@
�@
�H@
��@
�!@
�!@
~�@	��@	hs@	X@	&�@Ĝ@r�@A�@1'@1'@b@�w@�w@�P@l�@�@ȴ@v�@ff@V@V@V@V@V@V@V@E�@{@@p�@�@�@`B@O�@O�@�@��@�@�/@��@�j@��@�D@�D@Z@Z@(�@�
@ƨ@�F@�@dZ@"�@"�@@��@�\@^5@=q@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��+A��+A��+A��+A��7A��DA��DA��DA��PA��PA��\A��\A��hA��hA��uA��DA��A�hsA��A��A���A�$�A�XA�l�A��A��A��A���A��\A�7LA��-A���A��A�+A�/A�+A��TA�p�A�;dA��A��FA�-A��-A�dZA���A�C�A��;A�t�A��A�XA���A���A��+A�ZA���A���A�`BA�ffA���A�r�A�-A�Q�A�O�A��A�A���A��9A�A�A~5?A}A|^5Azz�Ay
=Axr�Aw�Au�PAs�ArM�ApZAo"�An�An��AnI�Al�Ak��Aj��Aj�uAjbNAjE�Aj�Ai�^AihsAh�Ag\)Ae��AdbAbQ�AaƨA`I�A^�A]l�A\ffA\�AZZAX�9AW�hAVjAUK�ATn�AS��AS/ARVAQ��AQ33AP��AP1AO�AN�AMoALM�AKƨAJ��AIK�AGhsAE��AD��ADJAC��AC7LAB��ABI�AA�AAhsAAK�AAVA@I�A?G�A=G�A;�
A:�+A:VA9�A8$�A7��A7�A6�!A69XA4��A3�;A3C�A2v�A1`BA0��A0��A0^5A0 �A/�TA/��A/A.$�A-S�A,�+A,A�A,$�A, �A, �A, �A,�A+�#A+;dA+�A*��A)�A(��A(1A'�hA'"�A&ȴA%��A%+A$Q�A#p�A"M�A!�A!+A�;Ap�AK�A��A��A�+AM�AJA�A;dA9XA�A��A�uAJA�AK�A%A��AO�A �A�RA+A~�A�7A+A�+AO�A$�A
ffA��AjA�A  A��A�A/AVAG�A�TA��A�+A�TA�^AS�A  �@���A {@���@�v�@���@��@�/@�Q�@��@�{@���@��@�r�@�E�@��@��@�hs@�?}@��@���@�|�@���@�~�@�p�@�@�ff@�bN@�@��@�@ߍP@�{@��/@���@���@��#@�X@���@��@�1'@���@�V@�l�@�$�@���@�O�@̋D@��H@��/@���@�K�@��@�"�@��#@��@�9X@�@�@�?}@�(�@�@���@�&�@�K�@�V@��#@���@�O�@�V@�1'@��P@���@�v�@�x�@�j@�A�@�"�@��9@�b@�\)@��@�@�`B@��`@��9@���@��u@��@�z�@�z�@�z�@�z�@�bN@�A�@� �@���@���@�33@��H@���@�V@�X@��9@�Q�@�t�@�{@��h@�X@�O�@�7L@�V@��@�\)@��H@�ff@�x�@��`@�j@�S�@���@�V@��@���@��/@�z�@��;@�C�@�~�@���@��@�r�@��m@��P@�C�@���@���@�V@���@���@��^@�x�@�&�@�%@��/@�z�@�Q�@�b@���@�|�@�C�@�
=@��+@���@�&�@��@���@�"�@�o@���@��T@���@�hs@�%@��9@�A�@��m@��P@���@�-@��#@�p�@��@�V@���@�Z@��@��F@���@��@�l�@�\)@�K�@�C�@�33@�"�@���@�ȴ@��!@��+@�^5@���@��@�%@��D@�1'@�1@�@��@�@|�@K�@�@~�y@~�@~��@~E�@~$�@}�@}��@}�@}p�@}O�@}?}@}/@|��@|9X@{�@z�H@z��@z�\@z^5@yx�@x �@w�P@wl�@w�@v�@v�R@v��@v��@w
=@w�@v�y@v�R@vv�@v$�@u��@uO�@tz�@s�m@so@r��@r��@r~�@r^5@rM�@r=q@r-@q��@qhs@p��@pĜ@p�u@pr�@p1'@pb@o�;@o�w@o��@n��@nȴ@n�R@n�R@n��@n��@n��@n�+@nE�@m�T@m��@o
=@o|�@o+@nV@l�@k@jn�@j-@j-@jJ@ix�@h�`@gK�@g
=@g\)@h  @i&�@i%@hbN@g�@g;d@fȴ@f�+@fV@fE�@f$�@e�@e��@e@e��@e��@e`B@eO�@d�@d9X@cS�@b�H@b�@b��@bM�@b=q@a�^@aG�@a7L@`�9@` �@_�;@_�P@_\)@_\)@_�P@_\)@]��@]O�@]O�@\��@\�D@[ƨ@[S�@[o@[@Z��@Z�\@Z~�@ZM�@Z=q@Z-@ZJ@Y��@Y%@X�9@Xr�@XA�@Xb@W�;@W�;@W�;@W�@W�;@W|�@V��@V5?@Up�@UO�@UV@T�j@T�D@Tj@TI�@T(�@S�m@St�@S33@So@R��@R�@Q�^@Q��@Q��@QX@Q�@P�`@PĜ@P1'@O��@Ol�@O+@N��@N��@Nv�@NE�@N$�@M��@M�-@Mp�@L��@L��@L�D@K"�@JM�@J-@J�@I�#@I�7@H��@G�;@G�w@G��@GK�@G;d@G;d@G�@G+@G+@G
=@G
=@F�y@F�@F5?@E�-@E�h@Ep�@E`B@E?}@EV@D�D@D�@C��@B�\@BM�@B-@A�@A��@Ahs@AG�@A�@@�9@@bN@@b@?�@?l�@>�y@>��@>V@>E�@>{@=�@=�@=?}@=/@=�@=V@<�@<��@<�j@;��@;C�@:n�@:�@9��@9G�@9�@8��@8��@8��@8�@8bN@8bN@8bN@8b@7l�@7+@6��@6��@6�+@6v�@6ff@6V@6$�@6@5@5`B@5�@4�/@4I�@4(�@3�m@3��@2�H@2n�@1��@1��@1X@0�`@0Ĝ@0�@01'@/��@/��@/;d@/
=@.ȴ@.��@.{@-@-p�@,�@,�@,z�@,I�@,�@+��@+�
@+ƨ@+��@+t�@+dZ@+"�@*�H@*��@*��@*~�@*^5@*=q@)�@)�@)��@)��@)��@)��@)x�@)hs@)X@)�@(�`@(��@(�9@(�@(A�@(1'@'�@'l�@'K�@';d@'+@'+@&ȴ@&��@&v�@&ff@&V@&V@&E�@&5?@%�@%�-@%�h@%�@%?}@%�@$�/@$��@$z�@$Z@$(�@#�
@#�F@#��@#��@#dZ@#33@#@"�\@"^5@"-@!�^@!7L@ ��@ ��@ �u@ bN@ 1'@�;@��@;d@�R@��@v�@E�@�T@`B@�@��@�D@z�@Z@9X@�@�
@t�@33@@�\@^5@�@�^@�7@x�@�@ �@�@+@�@��@v�@V@$�@�T@@�@O�@�@�D@Z@�@��@�
@dZ@o@@�@��@=q@hs@7L@�@��@�`@��@��@Ĝ@�9@��@r�@�;@ȴ@�R@��@E�@$�@{@��@�h@?}@?}@p�@p�@`B@O�@�@��@��@9X@�
@��@��@�@�@t�@t�@dZ@S�@S�@S�@33@"�@"�@
�@
�H@
��@
�!@
�!@
~�@	��@	hs@	X@	&�@Ĝ@r�@A�@1'@1'@b@�w@�w@�P@l�@�@ȴ@v�@ff@V@V@V@V@V@V@V@E�@{@@p�@�@�@`B@O�@O�@�@��@�@�/@��@�j@��@�D@�D@Z@Z@(�@�
@ƨ@�F@�@dZ@"�@"�@@��@�\@^5@=q@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B'�B(�B(�B'�B(�B'�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B'�B'�B'�B(�B(�B'�B$�B�B{BVB+B��B�;BƨB�B�PBaHB#�B�BÖB��B�1By�B[#BL�BL�BG�B7LB2-B8RB49B;dB8RB1'B+B&�B!�B�BhB\BJBDB%B
��B
�B
�;B
��B
ɺB
ɺB
ÖB
�dB
�B
�B
��B
��B
��B
��B
�JB
�B
~�B
t�B
iyB
dZB
\)B
O�B
E�B
;dB
2-B
+B
)�B
'�B
"�B
�B
{B
\B
\B
PB
JB
DB
+B
B	��B	�B	�B	�5B	��B	��B	ȴB	�wB	�LB	�!B	�B	��B	��B	��B	�JB	�+B	�B	�B	}�B	y�B	u�B	r�B	m�B	iyB	cTB	_;B	W
B	R�B	P�B	F�B	;dB	1'B	)�B	+B	&�B	&�B	$�B	"�B	 �B	�B	�B	�B	�B	hB	JB��B��B��B��B��B�B�B�B�B�B�`B�TB�HB�/B�B�B�B��B��B��B��B��BǮBĜBBŢBŢBƨBŢBŢBĜBB�}B��B�wB�dB�LB�3B�'B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�VB�=B�\B�VB�JB�1B�B�B�B�B�B� B� B}�Bz�Bs�Bm�BhsBffBgmBffBiyBe`B_;B[#BVBP�BYBXBXBW
BT�BS�BO�BM�BH�BF�BB�BE�BI�BF�BD�BL�BM�BJ�BH�BJ�BH�BH�BE�BC�BC�BA�B?}B5?B8RB>wB?}B?}B>wB<jB49B5?B9XB8RB5?B1'B1'B/B2-B33B1'B/B/B/B/B0!B1'B2-B/B/B/B2-B2-B33B7LB:^B9XB7LB6FB7LB<jB=qB;dB9XB>wBA�BA�BA�BA�BC�BB�BD�BE�BE�BD�BH�BJ�BK�BK�BK�BI�BK�BL�BL�BL�BN�BP�BT�BS�B[#B\)B[#BaHBaHBaHBdZBdZBe`Be`Be`Be`Be`Be`BdZBdZBdZBdZBcTBdZBdZBe`BdZBcTBgmBjBiyBk�Bq�Bt�Bu�Bu�Bt�Bs�Br�Bx�By�Bz�B~�B�B�B�7B�bB�hB�uB�uB��B��B��B��B��B��B�B�B�3B�LB�XB�jB�wB�}BÖBƨBǮBȴB��B��B��B��B��B��B�
B�B�#B�#B�5B�NB�`B�fB�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	%B		7B	JB	PB	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	'�B	2-B	1'B	5?B	:^B	;dB	<jB	<jB	=qB	>wB	?}B	@�B	@�B	A�B	A�B	C�B	D�B	D�B	F�B	G�B	G�B	H�B	H�B	G�B	H�B	K�B	N�B	Q�B	R�B	Q�B	Q�B	R�B	W
B	YB	ZB	[#B	_;B	cTB	e`B	ffB	gmB	hsB	iyB	k�B	l�B	m�B	m�B	o�B	s�B	u�B	y�B	{�B	{�B	|�B	|�B	}�B	}�B	}�B	}�B	� B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�7B	�VB	�\B	�bB	�bB	�bB	�bB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�XB	�qB	�}B	��B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�B	�B	�/B	�/B	�/B	�;B	�5B	�;B	�BB	�BB	�HB	�HB	�fB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
1B
1B
1B
	7B
	7B

=B
DB
JB
VB
VB
VB
VB
PB
PB
\B
\B
\B
\B
VB
VB
PB
PB
PB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
!�B
"�B
!�B
!�B
"�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
+B
,B
-B
-B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
2-B
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
5?B
49B
5?B
6FB
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
:^B
;dB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
O�B
N�B
M�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
XB
XB
XB
XB
XB
YB
XB
XB
XB
XB
VB
VB
ZB
[#B
ZB
[#B
[#B
[#B
[#B
[#B
]/B
^5B
_;B
_;B
_;B
^5B
_;B
_;B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
`BB
bNB
cTB
cTB
cTB
cTB
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
gmB
gmB
gmB
iyB
iyB
iyB
iyB
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
k�B
k�B
l�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B'�B(
B'�B'�B(
B'�B'�B(
B'�B(
B'�B'�B)B'�B(�B(�B'�B)B'�B'�B'�B(
B'�B'�B'�B(�B)B(�B(�B(�B'�B'�B'�B)B)*B($B%`B�B2B�B�B��B�|B��B�vB��Bh�B+6B��BȀB�B�DB~BaHBP�BNBI�B<B5�B9�B5�B<jB9rB2�B,"B(
B"�B�B�BHBB�B+B
��B
�;B
�B
�B
�^B
�XB
ĶB
��B
�!B
��B
�eB
�mB
�hB
�/B
��B
��B
�4B
v�B
k6B
e`B
^B
RB
G�B
=VB
4TB
,WB
*KB
(XB
#�B
kB
�B
}B
�B
�B
�B
�B
�B
�B	�jB	��B	�qB	�vB	��B	� B	ʦB	�OB	�	B	�[B	��B	�B	��B	��B	��B	��B	�9B	��B	~�B	z�B	v�B	s�B	ncB	jB	d�B	`BB	X�B	S�B	Q�B	H1B	=�B	3�B	,B	,B	'�B	'�B	%zB	#nB	!�B	~B	B	�B	KB	�B	�B	�B��B�>B�B��B�nB�nB�B�aB�B�RB�tB�4B�OB�KBּB�mBՁB�[B�oB�vB̳B�BżB�{B��B��B��BżBżB��B�B�4B��B�cB�jB��B�TB��B��B��B�DB��B�B�B�)B��B�qB�+B�EB�B�aB��B��B��B��B��B�lB��B�B��B��B��B��B��B~�B{�Bu�BoOBjBhXBh�Bg�Bj0Bf�BaB\�BX_BR�BYBX�BX_BWsBU�BT�BQ4BOvBJXBG_BC�BFtBJ=BGzBF%BMBNBK^BI�BKDBIBI7BF�BD�BDMBBAB@�B8RB9�B>�B?�B?�B>�B=<B6FB6�B9�B9	B6+B2|B2-B0�B2�B4B2aB0�B0!B0!B0B0�B2B2�B0UB0�B0�B3MB3�B4TB8B:�B9�B88B7�B8�B=<B>BB<�B;JB?cBB'BBuBBuBB�BDBC�BE�BFtBFtBE�BIlBK)BLBL0BL0BJ�BLJBMjBMPBM�BO�BQhBVBU�B[�B\�B[�Ba�Ba�Ba�Bd�Bd�Be�BezBezBezBe�BezBd�Bd�Bd�Bd�Bc�Bd�Bd�Be�Bd�BdBg�BkBjKBlqBq�Bt�Bu�BvBu%BtnBs�By>BzxB{�B}B��B��B��B��B��B��B�,B��B�7B�CB�jB��B�eB��B�wB��B��B��B��B��B��BðB��B��B�B��B��B�B�B�&B�MB�YB�kBیB��B��B��B��B�B��B��B�%B�^B�6B�PB�0B�6B�jB�6B�jB��B	 iB	aB	tB		�B	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	!�B	#B	#:B	$ZB	(�B	2aB	1�B	5�B	:�B	;�B	<�B	<�B	=�B	>�B	?�B	@�B	@�B	A�B	A�B	C�B	D�B	D�B	F�B	G�B	G�B	H�B	H�B	G�B	IB	L0B	O(B	R B	S&B	R:B	RTB	SuB	W?B	YKB	Z7B	[=B	_VB	cTB	ezB	f�B	g�B	h�B	i�B	k�B	l�B	m�B	m�B	pB	tB	v+B	zB	|B	|B	}B	}B	~(B	~B	~B	~(B	�4B	�GB	�3B	�SB	�SB	�_B	�_B	�KB	�lB	��B	�pB	�vB	�bB	��B	�}B	�}B	��B	�}B	��B	�{B	�eB	��B	�B	�'B	�IB	�)B	�B	��B	��B	�
B	�QB	�WB	��B	�;B	�9B	�$B	�<B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	� B	�B	�@B	�[B	�@B	�FB	�
B	�EB	�?B	�EB	�eB	�QB	�IB	�dB	�~B	�VB	�OB	�VB	�\B	�BB	�|B	��B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�	B	�	B	�B	��B	��B	�	B	�	B	��B	��B	�B	�B	�B	�B	�B	�6B	�<B	�.B	�.B
 B
 B
;B
 B
;B
 B
'B
;B
UB
-B
AB
�B
gB
YB
YB
?B
YB
�B
tB
KB
KB
KB
	RB
	RB

XB
^B
dB
pB
pB
pB
pB
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
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
B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
"�B
!�B
!�B
"�B
$�B
$B
$�B
%B
%B
%FB
&B
'B
'B
(
B
(>B
)B
)B
)B
*B
*B
*0B
+B
+B
,=B
+QB
,"B
-)B
-]B
./B
/OB
/OB
/OB
0;B
0;B
1'B
1[B
1[B
1[B
1AB
2aB
2GB
3hB
2GB
3hB
3MB
3MB
49B
49B
49B
4nB
4nB
4nB
5ZB
5ZB
4nB
5ZB
6FB
5tB
5tB
5tB
6`B
6zB
6�B
7�B
8lB
8RB
8lB
7�B
8�B
9rB
9rB
9XB
9rB
9rB
9�B
9rB
9rB
:�B
:�B
:xB
;�B
:xB
;�B
<�B
<�B
<�B
<�B
=�B
>�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
I�B
J�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
L�B
L�B
MB
NB
O�B
OB
N"B
O(B
PB
QB
RB
RB
RB
RB
RB
R B
RB
S&B
S&B
R:B
S&B
UB
UB
UB
UMB
UB
VB
VB
V9B
U2B
UgB
X+B
X+B
X+B
X+B
XB
Y1B
XEB
X+B
XEB
XEG�O�G�O�B
Z7B
[=B
ZQB
[=B
[=B
[WB
[WB
[=B
]/B
^5B
_VB
_;B
_;B
^jB
_pB
_pB
^jB
_VB
`\B
abB
aHB
aHB
abB
aHB
aHB
abB
aHB
aHB
abB
abB
aHB
abB
bhB
bNB
bhB
bNB
abB
`�B
b�B
c�B
cnB
cnB
cnB
d�B
ezB
e`B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
g�B
g�B
g�B
i�B
iyB
i�B
iyB
i�B
i�B
i�B
iyB
i�B
iyB
j�B
j�B
j�B
j�B
j�B
jB
j�B
j�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712190034172017121900341720171219003417201806221234542018062212345420180622123454201804050431082018040504310820180405043108  JA  ARFMdecpA19c                                                                20171215093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171215003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171215003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171215003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171215003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171215003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171215003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171215003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171215003516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171215003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20171215005541                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171215153503  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20171218000000  CF  PSAL_ADJUSTED_QCD�@ D݀ G�O�                JM  ARCAJMQC2.0                                                                 20171218153417  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171218153417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193108  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033454  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121519                      G�O�G�O�G�O�                