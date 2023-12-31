CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-03-09T15:36:02Z creation;2018-03-09T15:36:05Z conversion to V3.1;2019-12-18T07:24:25Z update;2022-11-21T05:31:07Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        \  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  MT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180309153602  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_130                     2C  DdEANAVIS_A                         0397                            ARGO 011514                     863 @�R"�Q��1   @�R#��� @<,�Q��dEA��s1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@�Q�@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B8
=B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?�\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW�\CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�9�D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD� RD�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D���D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A��uA��\A��7A��A��A��A��DA���A���A���A���A���A���A��A��A��A���A��A��A��A��A��A��A���A���A��A�x�A�7LA�ZA���A��
A���A���A�r�A�-A�A��wA�^5A�-A�-A��-A�G�A�hsA��-A��\A�\)A���A��yA�ƨA��A�oA���A��PA��-A�A�A��/A�$�A�/A���A�Q�A���A��A��HA���A�~�A���A���A�`BA��A�$�A�1'A~�A~��A~�\A}�A}`BA|�DA{O�Az1'Ax��AxJAv=qAt��Ar�/ApȴAp  An9XAmC�AlȴAk�Ak/Ak%Ajv�Ai
=Ah��Ah1AgXAf��Aep�Ac��AcXAcAb�\Aa�#A`�RA^�!A]t�A\��A[��A[l�A[\)A[�AZ^5AYS�AW�TAVn�AU�7AT  AQ�;AQ��AQl�AQ&�APjAOƨAO�7AOK�AO
=AN�jAN-AMdZALz�AK��AK�7AJ�`AJ~�AJ5?AJ1AIG�AH��AH$�AG;dAE33AD�jADM�ADJAC��ACO�AC%AB�9ABn�AB{AA�TAA�hA@n�A?��A>�A>I�A=�^A<�A;�TA;�A;�7A;+A:jA:1'A9��A9"�A7�A7��A77LA6��A6Q�A5�#A4��A49XA3|�A2�`A0�uA.��A-�FA-x�A,��A,�A+C�A*��A*��A*M�A)�hA(ȴA(bNA'�7A&(�A&  A%�^A%�PA%VA${A#C�A"�!A!�A �+A��A��A��A�FAhsA+A��A�
At�Ar�A`BA�HA��A�A�/A��A�TA&�A��A-AdZAVA�AȴAv�A�RA��A�`A%A	�hA1A��AdZA\)A\)A\)A�A�yA�HAȴA9XAG�Az�A�A-A��A7LA �A �uA Q�A 9XA  �@��m@���@�O�@�C�@��+@���@���@�dZ@�$�@�Z@땁@�;d@�R@�x�@��
@�p�@�Z@�|�@��@���@�M�@���@���@�1@ߍP@�o@އ+@��@ڗ�@�r�@�-@��@�n�@�Ĝ@��@��@ͺ^@Ͳ-@ͩ�@Ͳ-@͉7@͉7@�x�@�hs@�O�@̣�@��@ʸR@��@�  @�b@��@�dZ@���@�@��@��/@�I�@��w@�\)@��!@���@���@��m@��F@��P@�33@��@�5?@��7@��`@��w@�/@��@��@���@���@�?}@��@��j@��D@��@�|�@��@��R@�@�@�X@�bN@���@�1@��@��
@��w@��F@��P@�K�@�+@��@�$�@��@�dZ@��-@��u@��@��P@��H@�n�@�J@���@�G�@��@��u@�b@���@��y@�=q@�@�7L@��@���@��`@�Ĝ@�Q�@��
@�t�@��y@���@�E�@�@���@���@���@��@�7L@��9@�9X@�5?@��/@��@�I�@� �@�ƨ@���@�l�@�ȴ@�$�@��#@�p�@��`@��@�r�@�Q�@�9X@� �@���@��@�\)@�o@���@�ff@��-@��@�Z@�1@���@��@�ff@���@�hs@�%@���@��9@��@�j@��@��@��F@���@�|�@�l�@�l�@�\)@�;d@�"�@���@�ff@��^@�O�@�/@�&�@��@���@��9@��u@�z�@�@;d@�@~�R@|�/@|z�@|9X@{�
@{�@{dZ@{C�@{"�@z��@z��@z~�@zn�@zM�@z=q@zJ@y��@y��@y�#@y�^@y��@yhs@y7L@x��@x  @w�P@w�@vȴ@vE�@u�@u�-@u�@t9X@s�F@sdZ@so@r�\@r=q@q�#@q��@qhs@q7L@q%@p��@p�`@p��@p�9@p�@pbN@o|�@n�R@n$�@m�@m��@m��@m�h@m`B@l�/@lZ@l9X@k��@k�@kC�@j�H@j=q@i�^@i�7@ix�@ix�@ihs@iX@i�@hĜ@h�@g�@g\)@g
=@f�@fȴ@fv�@e�T@e?}@eV@d�@d��@dI�@cdZ@bM�@a&�@`Ĝ@`��@`��@`r�@_�;@_��@_;d@^�y@^�@^�+@^V@]�T@]��@]`B@]/@\�@\��@\�j@\�@\�D@\z�@\j@\Z@[��@[��@[dZ@[33@[o@[@Z�@[@Z�@Z�@Z�@Z�@Z��@Y��@Y&�@Y�@XĜ@XQ�@Xb@W�w@W+@W
=@V�y@Vv�@V$�@U/@T(�@S@R��@R~�@R^5@Q�#@Q7L@P�u@PA�@Pb@P  @P  @O�;@O��@O��@O�P@Ol�@Ol�@O�@N�y@N�+@N5?@M��@M@L��@L��@LI�@K��@K�F@Kt�@KC�@K33@J�@J�H@J��@J��@Jn�@J=q@J-@J�@J�@J�@JJ@JJ@JJ@JJ@I�@I�#@I��@I�@HbN@G�@G�;@G��@G�@G�@G��@G|�@Gl�@Gl�@G|�@G|�@G|�@G|�@G|�@Gl�@G+@F{@D�D@C"�@B��@B��@B~�@B^5@B=q@B=q@B=q@B-@Ax�@@b@?\)@?�@>��@>ȴ@>ff@>5?@>{@=�T@=�-@=�@<�j@<I�@<(�@<(�@<�@<�@;��@;ƨ@;S�@:��@:�\@:�\@:-@9�@9�^@9X@8��@8r�@7�@7|�@7\)@7+@6�@6ȴ@6v�@6$�@5�T@5�h@5`B@5?}@5/@5V@4��@49X@3�
@3�@3S�@3C�@3o@3@2�@2�!@2=q@2�@1hs@0�9@0�u@/�@/��@/K�@/�@.�y@.�y@.�R@.��@.��@.��@.��@.��@.��@.��@.��@.ff@.$�@-@-`B@,�@,9X@+�m@+��@+��@+t�@+dZ@+dZ@+dZ@+S�@+S�@+S�@+S�@+S�@+C�@+C�@+C�@+33@+"�@+o@+@*�H@*��@*��@*��@*�\@*^5@*^5@*-@)��@)��@)�7@)G�@(�u@'�@'|�@'K�@'
=@&ȴ@&��@&v�@&{@%`B@%O�@$��@$��@$z�@$Z@$9X@$1@#��@#�F@#t�@#"�@#"�@#@"�H@"�H@"�\@!��@ ��@ bN@  �@��@l�@K�@��@ȴ@��@ff@E�@E�@E�@5?@5?@5?@5?@�T@�@�@��@�/@��@Z@I�@�@�m@ƨ@��@t�@C�@@�H@�!@��@��@~�@n�@n�@=q@-@-@J@�#@��@�@�u@A�@��@�@��@ff@V@E�@{@��@?}@�/@�j@�@j@1@t�@"�@@��@M�@�@��@�@��@�7@�7@�7@hs@G�@&�@�9@�@r�@bN@Q�@A�@A�@A�@1'@�;@�P@�y@�+@5?@�@��@��@@�-@�h@�@p�@/@��@�@��@�D@�D@�D@�D@�D@�D@z�@j@Z@I�@9X@(�@�@1@��@�F@dZ@
�H@
�\@
-@
J@	�#@	��@	�7@	hs@	G�@�`@�@b@�@|�@\)@;d@��@��@@�T@��@�-@O�@V@V@�@�@��@��@z�@Z@9X@�
@�@�@t�@dZ@C�@@�H@��@��@��@�\@~�@n�@n�@n�@n�@-@J@�@��@�^@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A��uA��\A��7A��A��A��A��DA���A���A���A���A���A���A��A��A��A���A��A��A��A��A��A��A���A���A��A�x�A�7LA�ZA���A��
A���A���A�r�A�-A�A��wA�^5A�-A�-A��-A�G�A�hsA��-A��\A�\)A���A��yA�ƨA��A�oA���A��PA��-A�A�A��/A�$�A�/A���A�Q�A���A��A��HA���A�~�A���A���A�`BA��A�$�A�1'A~�A~��A~�\A}�A}`BA|�DA{O�Az1'Ax��AxJAv=qAt��Ar�/ApȴAp  An9XAmC�AlȴAk�Ak/Ak%Ajv�Ai
=Ah��Ah1AgXAf��Aep�Ac��AcXAcAb�\Aa�#A`�RA^�!A]t�A\��A[��A[l�A[\)A[�AZ^5AYS�AW�TAVn�AU�7AT  AQ�;AQ��AQl�AQ&�APjAOƨAO�7AOK�AO
=AN�jAN-AMdZALz�AK��AK�7AJ�`AJ~�AJ5?AJ1AIG�AH��AH$�AG;dAE33AD�jADM�ADJAC��ACO�AC%AB�9ABn�AB{AA�TAA�hA@n�A?��A>�A>I�A=�^A<�A;�TA;�A;�7A;+A:jA:1'A9��A9"�A7�A7��A77LA6��A6Q�A5�#A4��A49XA3|�A2�`A0�uA.��A-�FA-x�A,��A,�A+C�A*��A*��A*M�A)�hA(ȴA(bNA'�7A&(�A&  A%�^A%�PA%VA${A#C�A"�!A!�A �+A��A��A��A�FAhsA+A��A�
At�Ar�A`BA�HA��A�A�/A��A�TA&�A��A-AdZAVA�AȴAv�A�RA��A�`A%A	�hA1A��AdZA\)A\)A\)A�A�yA�HAȴA9XAG�Az�A�A-A��A7LA �A �uA Q�A 9XA  �@��m@���@�O�@�C�@��+@���@���@�dZ@�$�@�Z@땁@�;d@�R@�x�@��
@�p�@�Z@�|�@��@���@�M�@���@���@�1@ߍP@�o@އ+@��@ڗ�@�r�@�-@��@�n�@�Ĝ@��@��@ͺ^@Ͳ-@ͩ�@Ͳ-@͉7@͉7@�x�@�hs@�O�@̣�@��@ʸR@��@�  @�b@��@�dZ@���@�@��@��/@�I�@��w@�\)@��!@���@���@��m@��F@��P@�33@��@�5?@��7@��`@��w@�/@��@��@���@���@�?}@��@��j@��D@��@�|�@��@��R@�@�@�X@�bN@���@�1@��@��
@��w@��F@��P@�K�@�+@��@�$�@��@�dZ@��-@��u@��@��P@��H@�n�@�J@���@�G�@��@��u@�b@���@��y@�=q@�@�7L@��@���@��`@�Ĝ@�Q�@��
@�t�@��y@���@�E�@�@���@���@���@��@�7L@��9@�9X@�5?@��/@��@�I�@� �@�ƨ@���@�l�@�ȴ@�$�@��#@�p�@��`@��@�r�@�Q�@�9X@� �@���@��@�\)@�o@���@�ff@��-@��@�Z@�1@���@��@�ff@���@�hs@�%@���@��9@��@�j@��@��@��F@���@�|�@�l�@�l�@�\)@�;d@�"�@���@�ff@��^@�O�@�/@�&�@��@���@��9@��u@�z�@�@;d@�@~�R@|�/@|z�@|9X@{�
@{�@{dZ@{C�@{"�@z��@z��@z~�@zn�@zM�@z=q@zJ@y��@y��@y�#@y�^@y��@yhs@y7L@x��@x  @w�P@w�@vȴ@vE�@u�@u�-@u�@t9X@s�F@sdZ@so@r�\@r=q@q�#@q��@qhs@q7L@q%@p��@p�`@p��@p�9@p�@pbN@o|�@n�R@n$�@m�@m��@m��@m�h@m`B@l�/@lZ@l9X@k��@k�@kC�@j�H@j=q@i�^@i�7@ix�@ix�@ihs@iX@i�@hĜ@h�@g�@g\)@g
=@f�@fȴ@fv�@e�T@e?}@eV@d�@d��@dI�@cdZ@bM�@a&�@`Ĝ@`��@`��@`r�@_�;@_��@_;d@^�y@^�@^�+@^V@]�T@]��@]`B@]/@\�@\��@\�j@\�@\�D@\z�@\j@\Z@[��@[��@[dZ@[33@[o@[@Z�@[@Z�@Z�@Z�@Z�@Z��@Y��@Y&�@Y�@XĜ@XQ�@Xb@W�w@W+@W
=@V�y@Vv�@V$�@U/@T(�@S@R��@R~�@R^5@Q�#@Q7L@P�u@PA�@Pb@P  @P  @O�;@O��@O��@O�P@Ol�@Ol�@O�@N�y@N�+@N5?@M��@M@L��@L��@LI�@K��@K�F@Kt�@KC�@K33@J�@J�H@J��@J��@Jn�@J=q@J-@J�@J�@J�@JJ@JJ@JJ@JJ@I�@I�#@I��@I�@HbN@G�@G�;@G��@G�@G�@G��@G|�@Gl�@Gl�@G|�@G|�@G|�@G|�@G|�@Gl�@G+@F{@D�D@C"�@B��@B��@B~�@B^5@B=q@B=q@B=q@B-@Ax�@@b@?\)@?�@>��@>ȴ@>ff@>5?@>{@=�T@=�-@=�@<�j@<I�@<(�@<(�@<�@<�@;��@;ƨ@;S�@:��@:�\@:�\@:-@9�@9�^@9X@8��@8r�@7�@7|�@7\)@7+@6�@6ȴ@6v�@6$�@5�T@5�h@5`B@5?}@5/@5V@4��@49X@3�
@3�@3S�@3C�@3o@3@2�@2�!@2=q@2�@1hs@0�9@0�u@/�@/��@/K�@/�@.�y@.�y@.�R@.��@.��@.��@.��@.��@.��@.��@.��@.ff@.$�@-@-`B@,�@,9X@+�m@+��@+��@+t�@+dZ@+dZ@+dZ@+S�@+S�@+S�@+S�@+S�@+C�@+C�@+C�@+33@+"�@+o@+@*�H@*��@*��@*��@*�\@*^5@*^5@*-@)��@)��@)�7@)G�@(�u@'�@'|�@'K�@'
=@&ȴ@&��@&v�@&{@%`B@%O�@$��@$��@$z�@$Z@$9X@$1@#��@#�F@#t�@#"�@#"�@#@"�H@"�H@"�\@!��@ ��@ bN@  �@��@l�@K�@��@ȴ@��@ff@E�@E�@E�@5?@5?@5?@5?@�T@�@�@��@�/@��@Z@I�@�@�m@ƨ@��@t�@C�@@�H@�!@��@��@~�@n�@n�@=q@-@-@J@�#@��@�@�u@A�@��@�@��@ff@V@E�@{@��@?}@�/@�j@�@j@1@t�@"�@@��@M�@�@��@�@��@�7@�7@�7@hs@G�@&�@�9@�@r�@bN@Q�@A�@A�@A�@1'@�;@�P@�y@�+@5?@�@��@��@@�-@�h@�@p�@/@��@�@��@�D@�D@�D@�D@�D@�D@z�@j@Z@I�@9X@(�@�@1@��@�F@dZ@
�H@
�\@
-@
J@	�#@	��@	�7@	hs@	G�@�`@�@b@�@|�@\)@;d@��@��@@�T@��@�-@O�@V@V@�@�@��@��@z�@Z@9X@�
@�@�@t�@dZ@C�@@�H@��@��@��@�\@~�@n�@n�@n�@n�@-@J@�@��@�^@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BcTBcTBdZBe`Be`Be`Be`Be`BdZBcTBcTBe`Be`BffBgmBffBffBffBffBffBgmBgmBgmBhsBiyBiyBe`BdZB]/BI�B<jB?}BB�BA�B<jB:^B:^B9XB49B/B�BhBDBB��B��B��B�B�B�B�yB�`B�NB�/B��B�wB��B�oB�Bn�BcTB]/BM�B.B�B  B
�B
�5B
��B
ȴB
��B
��B
��B
�PB
�VB
�PB
�=B
�%B
�B
w�B
n�B
dZB
`BB
S�B
E�B
9XB
.B
'�B
�B
�B
�B
bB
DB
	7B
B	��B	��B	��B	�B	�B	�TB	�B	��B	��B	��B	ǮB	��B	�?B	�B	��B	��B	��B	��B	��B	��B	�oB	�JB	�B	� B	x�B	o�B	m�B	l�B	k�B	hsB	dZB	cTB	aHB	`BB	^5B	[#B	W
B	R�B	O�B	M�B	I�B	F�B	D�B	C�B	@�B	>wB	:^B	5?B	-B	+B	)�B	(�B	&�B	%�B	#�B	!�B	 �B	�B	�B	�B	�B	{B	bB	PB	DB	+B	B	B	B	  B��B��B��B��B�B�B�B�B�B�sB�ZB�HB�5B�B��B��BȴBƨBB��B�qB�dB�^B�RB�FB�3B�-B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�DB�7B�+B�B�B� B|�Bx�Bs�Bq�Bp�Bo�Bm�Bk�BiyBhsBgmBffBe`BcTB`BB]/BZBVBR�BP�BO�BO�BO�BO�BN�BN�BM�BM�BL�BJ�BH�BG�BD�BC�BC�BB�BB�BA�BA�BA�B@�B?}B>wB=qB<jB<jB:^B8RB6FB5?B5?B5?B49B33B33B1'B1'B1'B1'B0!B0!B0!B/B/B/B/B/B.B-B.B.B.B/B/B1'B2-B49B49B49B5?B5?B5?B5?B5?B5?B49B5?B5?B6FB5?B5?B:^B>wB@�BA�BB�BB�BC�BC�BD�BD�BE�BG�BI�BJ�BK�BK�BK�BK�BL�BM�BN�BP�BT�BXBXBYBYBYBYBZBYBZB[#BZB\)B]/B]/B^5BbNBdZBcTBdZBdZBe`Be`Be`BffBffBffBhsBiyBl�Bq�Bu�Bw�Bx�Bz�B|�B}�B� B�B�B�B�1B�DB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�qBƨBȴBɺB��B��B��B��B��B�B�B�)B�BB�TB�TB�ZB�`B�`B�sB�yB�B�B�B�B��B��B	B	B	1B	JB	bB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	&�B	'�B	'�B	'�B	(�B	)�B	)�B	+B	/B	49B	8RB	9XB	9XB	:^B	:^B	<jB	=qB	=qB	B�B	D�B	E�B	F�B	O�B	P�B	Q�B	S�B	T�B	VB	VB	W
B	XB	YB	YB	ZB	ZB	ZB	[#B	[#B	[#B	\)B	\)B	]/B	^5B	_;B	aHB	cTB	e`B	gmB	hsB	jB	k�B	l�B	n�B	r�B	t�B	u�B	w�B	y�B	z�B	|�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�JB	�JB	�PB	�PB	�VB	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�?B	�RB	�qB	�wB	�wB	�wB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�/B	�/B	�5B	�;B	�;B	�NB	�ZB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
DB
VB
\B
\B
\B
\B
\B
bB
\B
\B
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
/B
/B
0!B
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
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
6FB
6FB
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
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
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
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
XB
YB
YB
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
_;B
_;B
_;B
_;B
`BB
`BB
aHB
bNB
bNB
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
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BcTBcTBdZBe`Be`Be`BeFBe`Bd@BcnBcTBe`Be`BffBg�BffBffBffBffBffBgmBgmBg�Bh�Bi�Bi�Be�Bf2Ba�BN�B>�BA�BEBC�B="B:�B;B:DB5%B1'B�B�BBgB�^B�lB�tB��B�!B�IB�eB�B�TB�!BοB��B��B�aB�Bp!Bd�B_pBQ�B0�BkBuB
�WB
�B
�BB
ʦB
�3B
�B
�B
��B
��B
�"B
�B
�_B
��B
yXB
p;B
e�B
bhB
U�B
HB
;�B
/iB
)�B
�B
B
�B
NB
�B

#B
�B	��B	��B	��B	�B	�OB	��B	خB	ԕB	ѷB	��B	�lB	��B	��B	�/B	��B	�&B	�B	�|B	��B	�#B	�FB	�"B	��B	�'B	z�B	pB	m�B	m)B	l�B	i*B	d�B	c�B	a�B	`�B	_B	\CB	XB	S�B	P�B	N�B	J=B	GB	E9B	D�B	AoB	?�B	<B	7LB	-�B	+�B	*eB	)yB	'�B	&LB	$ZB	"NB	!bB	;B	�B	B	yB	�B	NB	<B	dB	1B	�B	{B	�B	B�qB��B��B�8B�3B�AB�OB�CB�kB�B�zB�B߾B��B��B�B�RBǮB�GB��B�B��B�0B�rB�LB�B��B��B�qB�kB��B��B�>B�B��B�BB��B��B��B�TB��B��B��B�JB�=B��B��B�{B� B~�B{JBt�BrGBq�Bp�BncBl=BjBiBg�BgBffBezBa�B_!B\�BXBT�BQ�BPHBPBO�BP.BO\BO(BNBNpBM�BL0BJ#BIlBE�BD�BD3BCBCBA�BA�BA�BAB@�B?�B>�B="B=qB<jB;dB9>B6`B5�B5�B4�B4nB4�B2�B1�B1�B1�B0�B0�B0�B/�B/�B/�B/�B/�B/�B.�B/�B/�B/�B0oB0UB2aB2�B4�B4TB4TB5tB5�B5ZB5tB5�B5�B4�B5�B6`B7LB7B7�B<jB?cBA;BB'BB�BCBDBD3BEBE9BF�BHfBJ=BJ�BK�BL0BLJBL~BMjBN�BO�BR�BVBX�BX�BYeBYeBYeBYeBZkBY�BZ�B[qBZ�B\�B]~B]�B^�Bb�BdtBc�Bd�Bd�Be�Be�Be�Bf�Bf�BgRBiyBj�Bm�BraBvFBxRByXB{JB}VB~]B�OB�UB�{B��B��B��B�B��B�B��B��B��B�B�5B�BB�B�NB�,B�LB�8B�>B�*B�DB�0B�kB��B��B��B�BB�B��B�	B�)B�B�<B�vB�uB�_BڠBܬB�B�nB�B�tB�B��B�B��B��B��B�'B�hB�rB�]B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	# B	$&B	&B	'B	(
B	(
B	($B	)B	*0B	*KB	+�B	/�B	4�B	8�B	9rB	9rB	:�B	:�B	<�B	=�B	=�B	B�B	D�B	E�B	GEB	O�B	Q B	R B	T,B	U2B	VB	VB	W?B	XEB	Y1B	Y1B	Z7B	Z7B	Z7B	[WB	[WB	[=B	\]B	\CB	]dB	^jB	_�B	a�B	c�B	e�B	g�B	h�B	j�B	k�B	l�B	o B	r�B	uB	vB	xB	y�B	{B	}"B	~B	B	�4B	�4B	� B	�;B	�;B	�AB	�AB	�aB	�tB	�lB	�xB	�dB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�,B	�B	�
B	�*B	�*B	�0B	�=B	�5B	�5B	�UB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	� B	��B	� B	��B	�B	�B	�4B	�MB	�?B	�EB	�EB	�7B	�WB	�WB	�IB	�IB	ބB	�pB	ߤB	�B	��B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	�"B	�"B	�B	�<B	�.B
 B
'B
B
GB
-B
-B
3B
B
3B
B
B
3B
B
B
MB
MB
�B
�B
�B
�B
vB
vB
vB
vB
\B
}B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
!B
!�B
"�B
"�B
$B
$�B
$�B
%B
%�B
&B
'B
'B
'B
'B
(
B
(>B
)*B
*B
*B
+B
+6B
+B
+6B
+B
+6B
,=B
,qB
-CB
/OB
/OB
0;B
1AB
1[B
2GB
2-B
2aB
2GB
2-B
2GB
2GB
2-B
2GB
2-B
2-B
2aB
2aB
3hB
3hB
3�B
4nB
5tB
6`B
6`B
6`B
6FB
7fB
7LB
7LB
7LB
7LB
7fB
7fB
7LB
7fB
7LB
7LB
7fB
7LB
7fB
7fB
7fB
8RB
8lB
8�B
8�B
8lB
8lB
9rB
9rB
9rB
9�B
:�B
;�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
EB
E�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
KB
K�B
K�B
L�B
L�B
MB
L�B
M�B
M�B
M�B
NB
N�B
OB
OB
OB
O�B
Q B
P�B
Q B
Q B
Q B
P�B
Q B
RB
Q�B
RB
RB
RB
R B
S&B
S&B
T,B
U2B
UMB
VB
W$B
W$B
W?B
WYB
WYB
XEB
YKB
YKB
Y1B
YKB
ZQB
[=B
[WB
[qB
\]B
\CB
]IB
]IB
]dB
]dB
^OB
^5B
^OB
^jB
^jB
^OB
_pB
_VB
_;B
_VB
_;B
_;B
_VB
_pB
_VB
`�B
`vB
a|B
b�B
bhB
cnB
cTB
cnB
cTB
cnB
dZB
d�B
dtB
dtB
dZB
d�B
d�B
e`B
e`B
e`B
e`B
ezB
ezB
e`B
e`B
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
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803200033162018032000331620180320003316202211182133572022111821335720221118213357201804031939392018040319393920180403193939  JA  ARFMdecpA19c                                                                20180310003521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180309153602  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180309153604  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180309153604  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180309153605  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180309153605  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180309153605  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180309153605  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180309153605  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180309153605                      G�O�G�O�G�O�                JA  ARUP                                                                        20180309155513                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312153706  CV  JULD            G�O�G�O�F                JM  ARCAJMQC2.0                                                                 20180319153316  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180319153316  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103939  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123357  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                