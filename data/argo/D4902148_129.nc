CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-02-27T15:36:26Z creation;2018-02-27T15:36:29Z conversion to V3.1;2019-12-18T07:24:37Z update;2022-11-21T05:31:10Z update;     
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
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΐ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180227153626  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_129                     2C  DdBBNAVIS_A                         0397                            ARGO 011514                     863 @�O� � 1   @�O�β@�@<$��q�j�dBBZ�c 1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z=q@��@��A�\A>�\A^�\A~�\A�G�A�z�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Ds�D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;��D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ��DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWs�DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�y�D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A��A��A��A��A��A��A��A��-A��-A��-A��-A��9A��9A��FA��FA��RA��A�O�A�ĜA�/A�jA�p�A��/A���A�bNA��-A�
=A��A��uA��A�ƨA��A�^5A�hsA���A��yA�A��-A���A�^5A��^A��A��HA�5?A��A�jA���A�r�A�bA��RA�\)A� �A��9A�x�A�bA�XA�&�A��A�=qA��!A�"�A��A�`BA���A�(�A�"�A~�uA}A|��Az�yAz�RAz1AwAv��Au�Ar�Ap�DAo��An��AmS�Am
=Al�Ak"�Ah�Ag��Ae�PAc��Ab��Ab�Aat�Aa�Aa�Aa"�AaVA`��A_��A^z�A^  A];dA\��A\��A\VA[ƨAZ$�AY�AX��AX��AX=qAVjAU�7AU�AS�ASO�AS/AR�yAR�9ARjAR9XAQ�^APjAO�#AOO�AN~�AMS�AJĜAH^5AG
=AF�jAF��AF�\AFz�AFZAFA�AF�AE�AE��AEVADn�AD(�AC�TAC33AB�9AA�mA@~�A?�A?��A?S�A>9XA<I�A;l�A:$�A97LA8�jA8E�A8A7�FA7/A6=qA5x�A4�!A3�FA2�A2^5A2(�A1�^A1|�A1hsA1;dA0  A.��A-��A,�DA+��A+?}A*�/A*$�A)�7A)\)A)oA(��A'�mA'hsA&�`A%�A$�HA$jA#�A!�#A!+A v�A -A�A��A{A��A�9A�hA��AjA�A�#A`BA�RAA�AbA�mAA33A�/A=qA�wAp�AĜA�
A�uA�TAt�A�Ar�A�AĜAC�A5?A
��A
  A	\)A�9AM�A�A|�A?}A&�A��A��AjA�
AZA�-A�A��A�A�A �A��A ��A v�A �@�v�@��`@�b@�S�@�5?@���@�b@�33@��+@��#@�x�@�O�@��/@�Z@�@�-@��@�Q�@�!@�9X@�\)@��#@�(�@�P@��@�{@���@ؓu@���@ղ-@�hs@��@Լj@�Z@Ұ!@�hs@мj@ϝ�@̼j@��;@�t�@�"�@�V@Ǖ�@Ə\@�{@ŉ7@ļj@�b@�K�@���@��y@°!@�@�^5@�@��/@���@��@�V@���@���@�`B@�I�@��T@�`B@��@��D@�(�@��m@�;d@�-@�j@�M�@���@�/@��9@�Z@� �@�l�@�@��R@��+@�v�@��@�J@�x�@��@�~�@��-@��@���@���@��
@��@���@��@�Ĝ@��;@�C�@�ff@��@���@��^@�x�@�p�@�V@���@�  @��F@�\)@�o@���@�E�@��@��@�z�@�S�@�@�ȴ@��R@�ff@��#@���@�p�@�X@�&�@���@�A�@��@��@���@�|�@�S�@�o@���@��@���@�?}@��@��@��@�l�@���@��!@�n�@��@��@�@���@���@�x�@��@��u@� �@�K�@���@�5?@��^@�p�@�/@�V@��/@��j@���@��@�r�@�I�@���@�+@���@�ff@��7@�Ĝ@��@�bN@�Q�@�I�@�(�@��@�1@�  @��m@���@��F@���@��@�t�@�dZ@�"�@�ȴ@��\@�~�@�v�@�n�@�^5@�J@�x�@�`B@�G�@���@�I�@l�@~�y@~v�@}��@}/@|��@|Z@|9X@|(�@{�
@{S�@z��@y��@y��@y��@y�7@yhs@y&�@x��@xĜ@x�9@w�@vff@u��@up�@u`B@t�D@s33@rn�@rJ@qhs@pĜ@p�9@p�9@pQ�@o��@o�@o
=@nȴ@n�+@nff@nE�@n5?@n$�@m�@m@m@m@m�-@m�@m?}@m�@l�/@l��@lZ@l1@k�m@k�F@k�@kC�@k@j��@j�\@jM�@i&�@h��@hĜ@h��@h�@h  @g�P@g|�@g|�@gK�@g�@f�R@f$�@f@e@e�-@eO�@d�/@cC�@b�!@b~�@b^5@bM�@b�@a�@a��@a��@a��@a�^@`��@`1'@_�@_��@_l�@_+@^5?@\��@[��@Z�@ZJ@Y�^@Y�@XĜ@X�u@XQ�@X  @X  @W�;@W��@W�w@W�w@W�w@W�w@W�@W�@W��@W��@W�P@W�P@W|�@W|�@W�P@W|�@Wl�@Wl�@Wl�@Wl�@Wl�@W;d@Vȴ@U�@UV@Tz�@T1@Sƨ@S��@S��@S��@S�@SdZ@SdZ@SS�@SS�@SdZ@SC�@R�@R��@R~�@Rn�@RM�@Q��@Q�@Q�^@Q��@Qhs@QX@QG�@Q7L@Q7L@Q7L@Q�@P��@P�`@P��@Pr�@P  @O\)@N��@N5?@N{@M�@M@MV@Kƨ@J�H@JJ@IG�@I%@H��@HbN@HA�@H  @G�P@G
=@F��@F��@F�+@Fff@FV@FE�@F$�@F$�@F{@E�T@E�@E�@Dj@D(�@D1@D1@Cƨ@C��@Ct�@C33@C"�@C@B��@B^5@A�#@A&�@@��@@��@?�@?��@?�P@?|�@?;d@>�@>�+@>$�@=�@=�-@=?}@<�j@<�@;��@;S�@;o@:M�@9��@9X@8��@8bN@8b@7�@7�P@7�@6��@6ȴ@6��@6V@5�T@5�-@5�@5O�@5/@5�@5V@4��@4��@4��@4Z@4(�@41@41@3�m@3�
@3�
@3ƨ@3��@3C�@2�H@2��@2�!@2��@2n�@2J@1�#@1��@1x�@1hs@1hs@1G�@1G�@1G�@1G�@17L@1&�@1�@1%@1%@1%@0��@0�`@0Ĝ@0�u@0Q�@0 �@/�;@/��@/l�@/�@.ȴ@.�+@.E�@-��@-?}@,�@,�D@,j@,Z@,9X@,9X@,�@+dZ@*��@*n�@*M�@)��@)hs@)&�@(��@(�`@(Ĝ@(��@(  @&�y@&��@&v�@&v�@&5?@&{@%�T@%O�@$�@$j@#�
@#t�@#S�@#S�@#33@#o@#@#@"�@"��@"�\@"n�@"=q@"J@!X@ ��@ r�@ A�@ b@�;@��@K�@
=@�y@�@ȴ@�R@��@�+@ff@V@$�@@�T@�-@��@�@/@�D@1@�
@��@��@��@dZ@@��@��@�\@�\@�\@�\@�\@n�@^5@M�@J@��@X@��@�9@�@Q�@A�@ �@b@�@�;@�w@�P@|�@l�@K�@+@
=@�y@�@ȴ@�R@��@E�@?}@��@z�@1@�m@�F@�@dZ@"�@�H@�!@��@�\@n�@�@�@�^@x�@&�@��@�`@�9@r�@�;@l�@\)@\)@\)@�y@@p�@p�@`B@?}@/@�@�@V@��@��@�@�@��@��@�j@�@�D@�D@z�@j@I�@9X@(�@�@1@��@�
@ƨ@�F@��@dZ@
�!@
^5@
=q@
�@	�@	�^@	�7@	hs@	X@	G�@	G�@	7L@	&�@��@�9@�9@�9@�@bN@Q�@Q�@ �@b@�@�;@�w@��@|�@l�@\)@;d@
=@��@��@�+@ff@E�@�@��@�@p�@`B@`B@O�@O�@?}@/@�@��@�j@�@��@z�@Z@(�@1@1@��@��@�
@ƨ@��@��@�@dZ@S�@S�@S�@C�@33@"�@o@@�H@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A��A��A��A��A��A��A��A��-A��-A��-A��-A��9A��9A��FA��FA��RA��A�O�A�ĜA�/A�jA�p�A��/A���A�bNA��-A�
=A��A��uA��A�ƨA��A�^5A�hsA���A��yA�A��-A���A�^5A��^A��A��HA�5?A��A�jA���A�r�A�bA��RA�\)A� �A��9A�x�A�bA�XA�&�A��A�=qA��!A�"�A��A�`BA���A�(�A�"�A~�uA}A|��Az�yAz�RAz1AwAv��Au�Ar�Ap�DAo��An��AmS�Am
=Al�Ak"�Ah�Ag��Ae�PAc��Ab��Ab�Aat�Aa�Aa�Aa"�AaVA`��A_��A^z�A^  A];dA\��A\��A\VA[ƨAZ$�AY�AX��AX��AX=qAVjAU�7AU�AS�ASO�AS/AR�yAR�9ARjAR9XAQ�^APjAO�#AOO�AN~�AMS�AJĜAH^5AG
=AF�jAF��AF�\AFz�AFZAFA�AF�AE�AE��AEVADn�AD(�AC�TAC33AB�9AA�mA@~�A?�A?��A?S�A>9XA<I�A;l�A:$�A97LA8�jA8E�A8A7�FA7/A6=qA5x�A4�!A3�FA2�A2^5A2(�A1�^A1|�A1hsA1;dA0  A.��A-��A,�DA+��A+?}A*�/A*$�A)�7A)\)A)oA(��A'�mA'hsA&�`A%�A$�HA$jA#�A!�#A!+A v�A -A�A��A{A��A�9A�hA��AjA�A�#A`BA�RAA�AbA�mAA33A�/A=qA�wAp�AĜA�
A�uA�TAt�A�Ar�A�AĜAC�A5?A
��A
  A	\)A�9AM�A�A|�A?}A&�A��A��AjA�
AZA�-A�A��A�A�A �A��A ��A v�A �@�v�@��`@�b@�S�@�5?@���@�b@�33@��+@��#@�x�@�O�@��/@�Z@�@�-@��@�Q�@�!@�9X@�\)@��#@�(�@�P@��@�{@���@ؓu@���@ղ-@�hs@��@Լj@�Z@Ұ!@�hs@мj@ϝ�@̼j@��;@�t�@�"�@�V@Ǖ�@Ə\@�{@ŉ7@ļj@�b@�K�@���@��y@°!@�@�^5@�@��/@���@��@�V@���@���@�`B@�I�@��T@�`B@��@��D@�(�@��m@�;d@�-@�j@�M�@���@�/@��9@�Z@� �@�l�@�@��R@��+@�v�@��@�J@�x�@��@�~�@��-@��@���@���@��
@��@���@��@�Ĝ@��;@�C�@�ff@��@���@��^@�x�@�p�@�V@���@�  @��F@�\)@�o@���@�E�@��@��@�z�@�S�@�@�ȴ@��R@�ff@��#@���@�p�@�X@�&�@���@�A�@��@��@���@�|�@�S�@�o@���@��@���@�?}@��@��@��@�l�@���@��!@�n�@��@��@�@���@���@�x�@��@��u@� �@�K�@���@�5?@��^@�p�@�/@�V@��/@��j@���@��@�r�@�I�@���@�+@���@�ff@��7@�Ĝ@��@�bN@�Q�@�I�@�(�@��@�1@�  @��m@���@��F@���@��@�t�@�dZ@�"�@�ȴ@��\@�~�@�v�@�n�@�^5@�J@�x�@�`B@�G�@���@�I�@l�@~�y@~v�@}��@}/@|��@|Z@|9X@|(�@{�
@{S�@z��@y��@y��@y��@y�7@yhs@y&�@x��@xĜ@x�9@w�@vff@u��@up�@u`B@t�D@s33@rn�@rJ@qhs@pĜ@p�9@p�9@pQ�@o��@o�@o
=@nȴ@n�+@nff@nE�@n5?@n$�@m�@m@m@m@m�-@m�@m?}@m�@l�/@l��@lZ@l1@k�m@k�F@k�@kC�@k@j��@j�\@jM�@i&�@h��@hĜ@h��@h�@h  @g�P@g|�@g|�@gK�@g�@f�R@f$�@f@e@e�-@eO�@d�/@cC�@b�!@b~�@b^5@bM�@b�@a�@a��@a��@a��@a�^@`��@`1'@_�@_��@_l�@_+@^5?@\��@[��@Z�@ZJ@Y�^@Y�@XĜ@X�u@XQ�@X  @X  @W�;@W��@W�w@W�w@W�w@W�w@W�@W�@W��@W��@W�P@W�P@W|�@W|�@W�P@W|�@Wl�@Wl�@Wl�@Wl�@Wl�@W;d@Vȴ@U�@UV@Tz�@T1@Sƨ@S��@S��@S��@S�@SdZ@SdZ@SS�@SS�@SdZ@SC�@R�@R��@R~�@Rn�@RM�@Q��@Q�@Q�^@Q��@Qhs@QX@QG�@Q7L@Q7L@Q7L@Q�@P��@P�`@P��@Pr�@P  @O\)@N��@N5?@N{@M�@M@MV@Kƨ@J�H@JJ@IG�@I%@H��@HbN@HA�@H  @G�P@G
=@F��@F��@F�+@Fff@FV@FE�@F$�@F$�@F{@E�T@E�@E�@Dj@D(�@D1@D1@Cƨ@C��@Ct�@C33@C"�@C@B��@B^5@A�#@A&�@@��@@��@?�@?��@?�P@?|�@?;d@>�@>�+@>$�@=�@=�-@=?}@<�j@<�@;��@;S�@;o@:M�@9��@9X@8��@8bN@8b@7�@7�P@7�@6��@6ȴ@6��@6V@5�T@5�-@5�@5O�@5/@5�@5V@4��@4��@4��@4Z@4(�@41@41@3�m@3�
@3�
@3ƨ@3��@3C�@2�H@2��@2�!@2��@2n�@2J@1�#@1��@1x�@1hs@1hs@1G�@1G�@1G�@1G�@17L@1&�@1�@1%@1%@1%@0��@0�`@0Ĝ@0�u@0Q�@0 �@/�;@/��@/l�@/�@.ȴ@.�+@.E�@-��@-?}@,�@,�D@,j@,Z@,9X@,9X@,�@+dZ@*��@*n�@*M�@)��@)hs@)&�@(��@(�`@(Ĝ@(��@(  @&�y@&��@&v�@&v�@&5?@&{@%�T@%O�@$�@$j@#�
@#t�@#S�@#S�@#33@#o@#@#@"�@"��@"�\@"n�@"=q@"J@!X@ ��@ r�@ A�@ b@�;@��@K�@
=@�y@�@ȴ@�R@��@�+@ff@V@$�@@�T@�-@��@�@/@�D@1@�
@��@��@��@dZ@@��@��@�\@�\@�\@�\@�\@n�@^5@M�@J@��@X@��@�9@�@Q�@A�@ �@b@�@�;@�w@�P@|�@l�@K�@+@
=@�y@�@ȴ@�R@��@E�@?}@��@z�@1@�m@�F@�@dZ@"�@�H@�!@��@�\@n�@�@�@�^@x�@&�@��@�`@�9@r�@�;@l�@\)@\)@\)@�y@@p�@p�@`B@?}@/@�@�@V@��@��@�@�@��@��@�j@�@�D@�D@z�@j@I�@9X@(�@�@1@��@�
@ƨ@�F@��@dZ@
�!@
^5@
=q@
�@	�@	�^@	�7@	hs@	X@	G�@	G�@	7L@	&�@��@�9@�9@�9@�@bN@Q�@Q�@ �@b@�@�;@�w@��@|�@l�@\)@;d@
=@��@��@�+@ff@E�@�@��@�@p�@`B@`B@O�@O�@?}@/@�@��@�j@�@��@z�@Z@(�@1@1@��@��@�
@ƨ@��@��@�@dZ@S�@S�@S�@C�@33@"�@o@@�H@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�fB�yB�B�yB�ZB�#B��B��B�!B��B�Br�Bo�BgmBZBC�B5?B&�B{B��B�TB��BB��B�
BB�jB�RB�}B��B�\B�Bx�Br�Bl�BgmBP�B8RB#�B�BbBPB1B
��B
�B
B
�9B
��B
��B
��B
�7B
}�B
r�B
o�B
hsB
ffB
aHB
R�B
J�B
C�B
1'B
!�B
�B
�B
JB
	7B
%B	��B	�yB	�HB	��B	��B	ĜB	��B	�qB	�jB	�jB	�jB	�dB	�^B	�?B	�B	�B	��B	��B	��B	��B	��B	��B	�uB	�bB	�\B	�JB	�B	|�B	y�B	t�B	q�B	p�B	o�B	n�B	m�B	k�B	gmB	^5B	ZB	W
B	S�B	M�B	?}B	8RB	5?B	33B	33B	2-B	2-B	1'B	1'B	0!B	/B	-B	+B	(�B	&�B	%�B	"�B	�B	�B	�B	uB	hB	\B	
=B	B��B��B��B��B�B�B�B�B�yB�fB�NB�;B�#B�B�B�B�
B�B��B��B��BƨBB��B�qB�jB�XB�LB�?B�9B�3B�'B�B�B��B��B��B��B��B��B��B�{B�hB�\B�PB�DB�7B�%B�B�B�B�B~�B|�B{�B{�Bz�By�Bx�Bw�Bu�Bt�Br�Bp�Bn�Bk�BiyBhsBgmBe`BcTB`BB\)BZBW
BT�BS�BQ�BP�BP�BO�BN�BN�BM�BL�BK�BI�BH�BG�BG�BF�BE�BE�BC�BB�BA�B@�B?}B>wB=qB<jB;dB:^B:^B9XB8RB8RB7LB7LB7LB6FB5?B49B2-B1'B0!B/B/B.B.B.B.B-B+B)�B-B/B0!B/B/B/B.B0!B0!B0!B0!B33B33B33B33B2-B6FB7LB7LB8RB9XB:^B;dB<jB<jB<jB<jB<jB<jB>wB?}B@�BA�BA�BA�BA�BA�BF�BG�BH�BH�BI�BI�BI�BJ�BM�BQ�BS�BT�BVBW
BW
BYBZB[#B[#B[#B]/B\)B]/BbNBgmBiyBjBk�BjBm�Bp�Bq�Br�Bt�Bv�Bw�By�B� B�1B�JB�VB�VB�\B�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�9B�FB�FB�LB�XB�XB�^B�dB�wB��BBĜBȴB��B��B��B��B�B�B�B�#B�)B�/B�/B�5B�BB�ZB�mB�B�B��B��B��B��B��B��B	  B	B	B	B	B	%B	DB	PB	bB	�B	�B	�B	�B	 �B	 �B	!�B	"�B	"�B	"�B	#�B	$�B	$�B	&�B	&�B	&�B	'�B	)�B	-B	/B	/B	/B	0!B	0!B	33B	7LB	8RB	9XB	;dB	@�B	E�B	G�B	H�B	I�B	K�B	N�B	O�B	P�B	Q�B	R�B	T�B	W
B	\)B	]/B	]/B	]/B	^5B	_;B	`BB	`BB	`BB	cTB	jB	l�B	m�B	m�B	p�B	u�B	x�B	z�B	|�B	� B	� B	� B	�B	�B	�+B	�+B	�1B	�=B	�=B	�DB	�DB	�DB	�JB	�PB	�PB	�PB	�PB	�VB	�\B	�bB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�?B	�LB	�RB	�RB	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�wB	�}B	��B	��B	B	B	ŢB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�HB	�TB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
bB
hB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
49B
49B
6FB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
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
P�B
P�B
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
VB
VB
VB
VB
VB
VB
VB
VB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
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
r�B
r�B
r�B
r�B
r�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�mB�mB�B�mB�mB�B�mB�mB�mB�mB�B�mB�B�mB�mB�mB�mB�B�B�B�0B��B�B�2B�/B̈́B��B��B��B�GBshBp�Bi*B\�BESB7LB)_B�B�qB�B�MB�aB�VB�B�3B�qB�xBB��B��B��By�Bs�Bn/BjeBTB:�B%BB4B�B)B
�B
ۦB
��B
�B
��B
�vB
��B
�DB
�B
s�B
qAB
iB
g�B
c�B
T�B
L~B
GB
3�B
# B
5B
�B
�B

#B
�B	��B	�kB	�B	��B	�B	�mB	ªB	��B	��B	��B	��B	�B	��B	��B	��B	��B	�yB	�RB	��B	��B	��B	��B	�B	� B	�bB	�VB	�3B	}�B	{JB	utB	rB	qB	o�B	o B	n/B	l�B	h�B	_B	[	B	XyB	VB	Q B	BAB	9�B	5�B	3hB	3�B	2aB	2aB	1[B	1�B	0�B	/�B	-�B	+�B	)�B	'�B	&�B	#�B	!B	/B	?B	B	:B	B	dB	gB	 �B�B��B�tB�3B�aB��B��B�B�B�B�\BۦBڠBٴBؓB�sB��B՛BѝB�0B��BÖB�oB�(B�VB�B��B��B�B�9B��B�;B��B�B�B��B�HB��B��B�B��B��B�.B�<B��B��B��B��B��B��B��B�B}�B|PB|6B{JBz�By�Bx�BvzButBs�BrBp!BlqBj0BiDBh�Bf�Be,Bb4B]�B[�BXEBU�BT�BR�BQ�BQ�BPHBO(BOBBNpBMjBL�BK�BIlBG�BG�BGEBF�BF�BD�BC�BBABAoB@�B?}B>(B=<B<PB;JB;B:B8�B8�B7�B7�B7�B6�B6B5�B4TB2�B1vB0�B0B/OB/5B.�B/ B.}B-�B-B./B/�B0�B/iB/�B/�B/OB1'B0�B1[B1�B3�B3�B3�B49B3�B6�B7�B7�B8�B9�B:�B;�B<�B<�B<�B<�B=B=VB?HB@BA BA�BBBBBB�BB�BG+BH1BI7BIBJ#BJrBJ�BL0BOBBR�BTaBUgBVSBWYBW�BYeBZkB[qB[WB[�B]dB\�B^OBc:Bh
Bi�Bj�Bk�BkQBnIBqABrGBs�ButBwfBx�B{0B�UB��B��B��B��B��B��B��B��B��B��B��B�	B�CB�pB��B�>B�DB�0B�KB��B�IB�UB�oB��B��B��B�zB��B��B��B��B��B��B��B�B�B�9B�7B�"B�(B�NB�FB�SB�yB�QB�WB�]B�IB�dBޞB��B��B�$B�B�-B�%B�	B�B�"B�(B�.B	 B	 B	;B	[B	�B	�B	�B	�B	 B	
B	�B	�B	�B	 �B	 �B	!�B	#B	#B	#B	#�B	%B	$�B	'B	'B	'B	(>B	*KB	-CB	/5B	/5B	/OB	0;B	0oB	3�B	7fB	8�B	9�B	<B	AB	E�B	G�B	IB	J#B	K�B	N�B	PB	Q B	R:B	S@B	UgB	WsB	\]B	]IB	]dB	]dB	^OB	_pB	`vB	`�B	`�B	c�B	j�B	l�B	m�B	m�B	qAB	v+B	y	B	{JB	}"B	�B	�4B	�4B	�uB	�SB	�EB	�_B	�fB	�rB	�rB	�^B	�xB	�xB	�dB	�jB	�jB	��B	��B	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	�8B	�>B	�6B	�B	�"B	�WB	�]B	��B	��B	�fB	��B	��B	�lB	��B	�xB	�^B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	�?B	�B	�(B	�NB	�@B	�FB	�B	�$B	�+B	�EB	�1B	�1B	�7B	�B	�7B	�B	�7B	�B	�B	�=B	�=B	�#B	�=B	�#B	�#B	�#B	�=B	�CB	�)B	�CB	�)B	�]B	�CB	�xB	ބB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�*B	�*B	�dB	�BB
 OB
UB
GB
GB
3B
3B
3B
SB
SB
YB
?B
EB
_B
1B
fB
KB
1B
fB
KB
fB
	lB

�B
xB
dB
JB
~B
dB
jB
jB
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
�B
 �B
!�B
"B
#B
#�B
#�B
#�B
$B
%B
%�B
%�B
%�B
'B
'B
'B
'B
($B
($B
($B
)*B
)*B
(�B
)*B
*B
)�B
*0B
*B
*0B
+B
+B
,B
,=B
,"B
,"B
-CB
-CB
-CB
.B
./B
.IB
./B
.B
.B
./B
.B
./B
.B
./B
.B
./B
./B
.IB
./B
/OB
/5B
/5B
0UB
1AB
1AB
2GB
2aB
2GB
2aB
3MB
3MB
3hB
4TB
4TB
4TB
5ZB
4TB
4�B
6�B
7�B
8�B
8�B
9rB
9�B
:xB
:xB
:xB
:�B
:�B
;�B
=�B
=�B
=qB
=�B
>�B
>�B
>�B
?�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
FB
E�B
G�B
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
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
K�B
N"B
M�B
N�B
N�B
N�B
N�B
N�B
OB
N�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
PB
O�B
QB
QB
RB
SB
S&B
S&B
S�B
T,B
T�B
U2B
UB
UB
U2B
UB
T�B
U2B
U2B
V9B
VB
VB
VB
V9B
VB
VSB
VmB
X_B
XEB
YKB
Z7B
ZQB
ZQB
ZQB
[WB
[=B
[=B
\CB
\CB
\CB
\CB
\]B
\]B
]dB
]IB
^OB
^OB
^OB
^OB
^�B
_pB
`BB
`BB
`vB
`�B
`�B
b�B
cTB
cnB
cnB
cnB
cTB
dtB
dtB
dZB
dtB
dtB
dZB
dtB
dZB
dZB
dtB
dtB
dtB
e`B
e`B
e�B
ezB
e`B
e`B
ezB
e`B
e�B
ffB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
hsB
i�B
iyB
iyB
i�B
i�B
i�B
iyB
iyB
i�B
j�B
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
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
r�B
r�B
r�B
r�B
r�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803130037482018031300374820180313003748202211182133492022111821334920221118213349201804031939312018040319393120180403193931  JA  ARFMdecpA19c                                                                20180228003523  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180227153626  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180227153627  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180227153627  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180227153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180227153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180227153628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180227153628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180227153628  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180227153629                      G�O�G�O�G�O�                JA  ARUP                                                                        20180227155621                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180227153340  CV  JULD            G�O�G�O�F�}                JM  ARCAJMQC2.0                                                                 20180312153748  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180312153748  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103931  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123349  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                