CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-09-20T18:35:50Z creation;2017-09-20T18:35:53Z conversion to V3.1;2019-12-18T07:28:03Z update;2022-11-21T05:31:58Z update;     
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
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170920183550  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA  I1_0397_113                     2C  DdRNAVIS_A                         0397                            ARGO 011514                     863 @�'�)&N 1   @�'��Sp @;�F
�L0�dRT`�e1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B��B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�{A�G�B��B��B=qB��B'��B/��B7��B@
=BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D���D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}DܽD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�9�D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��/A��;A��;A��;A��#A��#A��#A��/A��/A��;A��;A���Aܣ�A܍PA܇+A܁AٍPAжFA���A��;A���A���A�ZAƉ7Aŧ�A��AA�bA���A��A�bNA�A���A��DA�v�A�z�A��A�\)A���A��9A���A�ĜA�  A���A�A��A�(�A���A��wA�ffA�jA�;dA���A��A��A���A���A��A��RA�C�A��A���A��A��wA���A�-A���A�M�A��;A�C�A�`BA��hA�A�XA�v�A��FA�ZA�ƨA��A�ffA�(�A�r�A�|�A��#A���A�ƨA~��A}%A|^5A{��Ay/Ax(�Aw�Av1As?}Aqt�Am�-AkdZAj��Ai��Ai&�Ah��Ag��AgƨAgp�AgoAf�yAfVAe�Ac�FAbjAa�7A`�A`E�A_�A\��AZ�AZbAY��AY�AY�AY��AY?}AX�DAXbAW�7AW7LAWVAVffAU�#AU|�AU�AT��ATA�AS��AR�AO�AOK�AM7LAL��AK��AK+AJ��AJ�jAJ��AJ�uAJ1'AI+AH5?AF�AD��AB��AB-AAO�A?��A>ȴA>I�A=��A=&�A<�/A<n�A;�A;hsA:�!A9K�A9�A8�A77LA6�A4��A1�
A0�!A/�A/O�A.�RA.ffA. �A-��A-p�A,�jA,�DA, �A+�A+��A+t�A+/A*�A*z�A)�TA);dA(ȴA'A'�PA'hsA'\)A'`BA'?}A&��A&=qA%`BA#&�A!�FA �jA �AG�A=qA�7Al�A`BA?}A�AI�Ax�AoAVAt�A��A�Ar�A1A��A�A-A��A"�Av�A1'A�A�FA�PAC�A��Ar�AJA`BA�yA��A&�A
�HA
r�A	��A	��A	dZA��A��AdZA�A
=A�+A�^AZAt�A�A �HA ��A   @��T@�G�@�-@�t�@�S�@��@���@��T@���@���@��;@��y@�Ĝ@��@�l�@�!@�E�@�`B@���@�|�@柾@�?}@�u@�K�@�v�@�O�@�Q�@��@�@�{@؛�@��@�ȴ@�J@�@�x�@�7L@���@ӶF@Ӆ@�+@҇+@���@��T@�@Η�@��/@�9X@�$�@§�@�7L@�A�@�|�@���@���@��-@��@�`B@�&�@�Ĝ@�(�@�33@���@�@�x�@�Q�@��@���@�33@�$�@���@���@��`@���@��@�Z@�I�@�1'@�b@��m@���@�l�@���@�^5@�@�1@�v�@�M�@�$�@�v�@��-@�G�@��@��@���@���@��j@��u@�r�@�9X@���@�l�@��@��^@���@��`@��j@��@��D@�A�@��@���@�C�@�v�@��@�hs@��;@��w@��F@�dZ@��^@���@�z�@��@���@��@��-@��7@�O�@���@�bN@�\)@�^5@�`B@��@���@���@��j@�j@��@��@���@�l�@�dZ@�\)@�\)@�S�@�;d@�;d@�;d@�o@���@��@��u@��@��
@���@�\)@�@�v�@��@���@���@���@��@�1'@�1@��@��w@���@��@�\)@�33@�ȴ@�=q@�{@��@�@��@�?}@��D@�bN@�A�@��@��@� �@��@�b@�1@�1@��
@�dZ@�o@��@�ȴ@���@�~�@�J@�?}@�j@� �@�w@�@~ȴ@~v�@}O�@{�
@{t�@{C�@{"�@{@z��@z�\@y�#@y�^@yG�@y�@x��@xĜ@xbN@w�;@wl�@vv�@u�T@u��@u��@up�@u/@tj@st�@r�@r-@q�^@qX@q&�@q%@p��@p��@p�`@pr�@p  @o��@o
=@n��@m�T@mO�@m/@m/@mV@l��@lj@lI�@l1@k�m@kƨ@k��@kC�@k33@k"�@ko@k@k@k@ko@k@j��@j��@j�!@j�!@j�\@jn�@jM�@jM�@j=q@i��@i�#@i��@i&�@h��@h��@i%@i%@h��@h�`@i&�@i�@h�@h �@g�@eO�@c�
@c�@co@bn�@a�@a&�@_�w@_�@^�y@^ȴ@^�+@^E�@]��@]V@\�/@\��@\�j@\�@\�D@\j@\I�@\I�@\(�@\�@[��@Z�!@Y�@Y��@W�w@V��@W
=@V��@Vff@V@U�-@U�@UV@T��@Tz�@T�@R�!@Q��@Qhs@Q�@Q%@P��@P�`@P��@PĜ@P�9@P��@P�u@PA�@O��@N��@Nȴ@Nȴ@Nȴ@Nȴ@N�@NE�@M@MO�@K��@KdZ@KC�@K33@J��@J-@JJ@I��@I��@I�^@I��@I��@Ihs@I%@H��@H�u@H1'@Hb@G�@G�w@F��@E?}@D�/@D�j@D��@C�m@C�@C�@CdZ@CdZ@CdZ@CS�@CS�@CC�@C33@C"�@B�H@B��@B��@B�!@B�\@B^5@B=q@A��@@�u@?�@?K�@>��@>�y@>ȴ@>��@>��@>�+@>�+@>E�@=�T@=��@=O�@<9X@;��@;�m@;�
@;�
@;ƨ@;ƨ@;��@;33@9��@9��@9X@8�`@8Ĝ@8��@8�u@8�u@8�u@8bN@8b@7l�@7K�@7K�@7;d@7�@6ȴ@6v�@5��@5p�@5/@4�D@4I�@4(�@4�@3�m@3ƨ@3��@3��@3�@3C�@2�@2��@2�@1�^@1hs@1�@0��@0�9@01'@/�w@/l�@/;d@/�@.�@.ȴ@.ȴ@.ȴ@.��@.V@-�@-�@,��@,��@,z�@,I�@+�m@+33@*-@)��@)�7@(�`@(��@(��@(��@(�u@(�@(�@(�@(�@(�u@(�@(�@(�@(�@(b@'�@'+@&��@&�@&ȴ@&��@&{@&{@%�@%�T@%��@%@%�-@%O�@%�@$��@$��@$9X@$1@#�F@#dZ@#33@#o@"�@"�H@"��@"��@"�!@"�\@"n�@"M�@"J@!��@!hs@!�@!%@!%@ ��@!%@ ��@ ��@ �u@ bN@ 1'@ 1'@�;@l�@+@
=@
=@��@�y@�R@��@��@�+@�+@v�@v�@v�@ff@ff@V@E�@$�@@��@/@��@�D@z�@j@Z@�@�
@��@t�@@��@��@��@�!@��@n�@-@��@��@��@�7@X@��@�u@Q�@1'@  @�@�P@;d@�@��@v�@E�@$�@{@{@�@��@�h@p�@p�@`B@O�@/@j@�@��@�m@�
@�F@C�@@��@^5@=q@M�@M�@M�@M�@-@hs@��@�9@��@r�@A�@�@��@�@�P@|�@l�@+@�@ȴ@�+@{@�@�T@@�h@O�@�@V@�@z�@��@S�@"�@"�@
�@
��@
��@
�!@
�!@
�!@
��@
��@
�\@
~�@
n�@	�@	�7@	X@	7L@��@��@��@�@Q�@1'@ �@ �@b@b@b@b@b@  @�@�@�;@�;@�@K�@
=@�+@E�@{@@@�@��@j@I�@�@��@�
@��@t�@S�@33@"�@"�@o@o@@�@��@�!@n�@�@J@��@�@�#@��@x�@hs@ ��@ b?�|�?���?�v�?�v�?�V?�V?�V?�5??�5??�{?�{?���?��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��/A��;A��;A��;A��#A��#A��#A��/A��/A��;A��;A���Aܣ�A܍PA܇+A܁AٍPAжFA���A��;A���A���A�ZAƉ7Aŧ�A��AA�bA���A��A�bNA�A���A��DA�v�A�z�A��A�\)A���A��9A���A�ĜA�  A���A�A��A�(�A���A��wA�ffA�jA�;dA���A��A��A���A���A��A��RA�C�A��A���A��A��wA���A�-A���A�M�A��;A�C�A�`BA��hA�A�XA�v�A��FA�ZA�ƨA��A�ffA�(�A�r�A�|�A��#A���A�ƨA~��A}%A|^5A{��Ay/Ax(�Aw�Av1As?}Aqt�Am�-AkdZAj��Ai��Ai&�Ah��Ag��AgƨAgp�AgoAf�yAfVAe�Ac�FAbjAa�7A`�A`E�A_�A\��AZ�AZbAY��AY�AY�AY��AY?}AX�DAXbAW�7AW7LAWVAVffAU�#AU|�AU�AT��ATA�AS��AR�AO�AOK�AM7LAL��AK��AK+AJ��AJ�jAJ��AJ�uAJ1'AI+AH5?AF�AD��AB��AB-AAO�A?��A>ȴA>I�A=��A=&�A<�/A<n�A;�A;hsA:�!A9K�A9�A8�A77LA6�A4��A1�
A0�!A/�A/O�A.�RA.ffA. �A-��A-p�A,�jA,�DA, �A+�A+��A+t�A+/A*�A*z�A)�TA);dA(ȴA'A'�PA'hsA'\)A'`BA'?}A&��A&=qA%`BA#&�A!�FA �jA �AG�A=qA�7Al�A`BA?}A�AI�Ax�AoAVAt�A��A�Ar�A1A��A�A-A��A"�Av�A1'A�A�FA�PAC�A��Ar�AJA`BA�yA��A&�A
�HA
r�A	��A	��A	dZA��A��AdZA�A
=A�+A�^AZAt�A�A �HA ��A   @��T@�G�@�-@�t�@�S�@��@���@��T@���@���@��;@��y@�Ĝ@��@�l�@�!@�E�@�`B@���@�|�@柾@�?}@�u@�K�@�v�@�O�@�Q�@��@�@�{@؛�@��@�ȴ@�J@�@�x�@�7L@���@ӶF@Ӆ@�+@҇+@���@��T@�@Η�@��/@�9X@�$�@§�@�7L@�A�@�|�@���@���@��-@��@�`B@�&�@�Ĝ@�(�@�33@���@�@�x�@�Q�@��@���@�33@�$�@���@���@��`@���@��@�Z@�I�@�1'@�b@��m@���@�l�@���@�^5@�@�1@�v�@�M�@�$�@�v�@��-@�G�@��@��@���@���@��j@��u@�r�@�9X@���@�l�@��@��^@���@��`@��j@��@��D@�A�@��@���@�C�@�v�@��@�hs@��;@��w@��F@�dZ@��^@���@�z�@��@���@��@��-@��7@�O�@���@�bN@�\)@�^5@�`B@��@���@���@��j@�j@��@��@���@�l�@�dZ@�\)@�\)@�S�@�;d@�;d@�;d@�o@���@��@��u@��@��
@���@�\)@�@�v�@��@���@���@���@��@�1'@�1@��@��w@���@��@�\)@�33@�ȴ@�=q@�{@��@�@��@�?}@��D@�bN@�A�@��@��@� �@��@�b@�1@�1@��
@�dZ@�o@��@�ȴ@���@�~�@�J@�?}@�j@� �@�w@�@~ȴ@~v�@}O�@{�
@{t�@{C�@{"�@{@z��@z�\@y�#@y�^@yG�@y�@x��@xĜ@xbN@w�;@wl�@vv�@u�T@u��@u��@up�@u/@tj@st�@r�@r-@q�^@qX@q&�@q%@p��@p��@p�`@pr�@p  @o��@o
=@n��@m�T@mO�@m/@m/@mV@l��@lj@lI�@l1@k�m@kƨ@k��@kC�@k33@k"�@ko@k@k@k@ko@k@j��@j��@j�!@j�!@j�\@jn�@jM�@jM�@j=q@i��@i�#@i��@i&�@h��@h��@i%@i%@h��@h�`@i&�@i�@h�@h �@g�@eO�@c�
@c�@co@bn�@a�@a&�@_�w@_�@^�y@^ȴ@^�+@^E�@]��@]V@\�/@\��@\�j@\�@\�D@\j@\I�@\I�@\(�@\�@[��@Z�!@Y�@Y��@W�w@V��@W
=@V��@Vff@V@U�-@U�@UV@T��@Tz�@T�@R�!@Q��@Qhs@Q�@Q%@P��@P�`@P��@PĜ@P�9@P��@P�u@PA�@O��@N��@Nȴ@Nȴ@Nȴ@Nȴ@N�@NE�@M@MO�@K��@KdZ@KC�@K33@J��@J-@JJ@I��@I��@I�^@I��@I��@Ihs@I%@H��@H�u@H1'@Hb@G�@G�w@F��@E?}@D�/@D�j@D��@C�m@C�@C�@CdZ@CdZ@CdZ@CS�@CS�@CC�@C33@C"�@B�H@B��@B��@B�!@B�\@B^5@B=q@A��@@�u@?�@?K�@>��@>�y@>ȴ@>��@>��@>�+@>�+@>E�@=�T@=��@=O�@<9X@;��@;�m@;�
@;�
@;ƨ@;ƨ@;��@;33@9��@9��@9X@8�`@8Ĝ@8��@8�u@8�u@8�u@8bN@8b@7l�@7K�@7K�@7;d@7�@6ȴ@6v�@5��@5p�@5/@4�D@4I�@4(�@4�@3�m@3ƨ@3��@3��@3�@3C�@2�@2��@2�@1�^@1hs@1�@0��@0�9@01'@/�w@/l�@/;d@/�@.�@.ȴ@.ȴ@.ȴ@.��@.V@-�@-�@,��@,��@,z�@,I�@+�m@+33@*-@)��@)�7@(�`@(��@(��@(��@(�u@(�@(�@(�@(�@(�u@(�@(�@(�@(�@(b@'�@'+@&��@&�@&ȴ@&��@&{@&{@%�@%�T@%��@%@%�-@%O�@%�@$��@$��@$9X@$1@#�F@#dZ@#33@#o@"�@"�H@"��@"��@"�!@"�\@"n�@"M�@"J@!��@!hs@!�@!%@!%@ ��@!%@ ��@ ��@ �u@ bN@ 1'@ 1'@�;@l�@+@
=@
=@��@�y@�R@��@��@�+@�+@v�@v�@v�@ff@ff@V@E�@$�@@��@/@��@�D@z�@j@Z@�@�
@��@t�@@��@��@��@�!@��@n�@-@��@��@��@�7@X@��@�u@Q�@1'@  @�@�P@;d@�@��@v�@E�@$�@{@{@�@��@�h@p�@p�@`B@O�@/@j@�@��@�m@�
@�F@C�@@��@^5@=q@M�@M�@M�@M�@-@hs@��@�9@��@r�@A�@�@��@�@�P@|�@l�@+@�@ȴ@�+@{@�@�T@@�h@O�@�@V@�@z�@��@S�@"�@"�@
�@
��@
��@
�!@
�!@
�!@
��@
��@
�\@
~�@
n�@	�@	�7@	X@	7L@��@��@��@�@Q�@1'@ �@ �@b@b@b@b@b@  @�@�@�;@�;@�@K�@
=@�+@E�@{@@@�@��@j@I�@�@��@�
@��@t�@S�@33@"�@"�@o@o@@�@��@�!@n�@�@J@��@�@�#@��@x�@hs@ ��@ b?�|�?���?�v�?�v�?�V?�V?�V?�5??�5??�{?�{?���?��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BȴBȴBɺBɺBɺBȴBȴBɺB��B��BɺB��B�B�yB�B�B�sB�BhB{BbBJB	7B%BB��B��B�B�BB�B��BÖB�wB�?B��B�JB�uB�+Bz�Br�BhsB_;BQ�BQ�BR�BM�BR�BL�B@�B8RB2-B�B��B��B�?B�!B��B��B��B��B��B��B��B��B�oB�7B�B~�Bx�Bq�BiyB^5BO�BE�B<jB33B(�B!�B�B  B
�B
�B
�;B
��B
ǮB
B
�-B
�oB
�B
� B
w�B
e`B
\)B
Q�B
F�B
2-B
$�B
VB
B	��B	�B	�B	�B	�mB	�`B	�NB	�BB	�5B	�B	��B	ÖB	�^B	�?B	�'B	�B	��B	�uB	�=B	�%B	�B	�B	�B	�B	�B	}�B	{�B	x�B	v�B	u�B	q�B	o�B	n�B	l�B	iyB	ffB	aHB	XB	K�B	E�B	9XB	49B	1'B	2-B	2-B	1'B	0!B	/B	,B	'�B	"�B	�B	VB	%B	B��B��B�B�B�B�B�B�sB�`B�NB�;B�B�B��B��B��BŢB��B�wB�jB�^B�RB�LB�FB�9B�3B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�+B�B�B}�Bz�Bx�Bw�Bw�Bv�Bu�Bt�Bs�Br�Bq�Bq�Bp�Bo�Bn�Bl�Bk�BiyBgmBdZBbNBaHBaHB`BB`BB_;B^5B^5B\)B[#BZBXBVBT�BS�BR�BR�BQ�BP�BO�BN�BL�BJ�BI�BG�BF�BD�BD�BC�BC�BB�B@�B@�B>wB<jB<jB;dB;dB:^B8RB7LB7LB6FB5?B5?B49B49B49B33B33B33B2-B2-B2-B2-B1'B1'B1'B0!B.B/B.B.B/B/B/B/B/B.B0!B0!B0!B0!B0!B/B/B-B/B/B+B/B2-B49B6FB7LB8RB:^B:^B:^B:^B:^B:^B:^B<jB<jBA�BB�BC�BB�BB�BA�B@�B@�B@�BC�BC�BD�BD�BE�BF�BG�BH�BI�BI�BI�BI�BH�BI�BH�BE�BI�BP�BZB]/B_;B`BBbNBdZBe`Be`Be`BffBffBgmBhsBk�Bm�Bm�Bm�Bm�Bm�Bo�Bp�Bs�Bs�Bt�Bu�Bu�Bz�B}�B~�B}�B~�B� B�B�B�7B�JB�PB�PB�VB�\B�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�!B�'B�9B�^B��BĜBƨBȴBɺB��B��B��B��B��B��B�B�B�B�B�#B�)B�)B�)B�/B�BB�ZB�`B�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	%B	+B	1B	
=B	JB	oB	�B	�B	�B	 �B	"�B	"�B	'�B	-B	.B	/B	/B	0!B	1'B	2-B	7LB	8RB	<jB	>wB	>wB	?}B	A�B	C�B	F�B	J�B	M�B	M�B	N�B	O�B	P�B	Q�B	S�B	T�B	XB	YB	ZB	[#B	\)B	\)B	\)B	\)B	^5B	_;B	_;B	dZB	gmB	jB	m�B	m�B	m�B	m�B	p�B	q�B	q�B	s�B	s�B	t�B	u�B	w�B	x�B	x�B	y�B	y�B	z�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�=B	�=B	�=B	�=B	�DB	�JB	�VB	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�9B	�?B	�LB	�^B	�dB	�jB	�jB	�qB	�wB	�wB	��B	��B	B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�HB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
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
	7B
	7B
	7B

=B

=B

=B
DB
PB
VB
bB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
"�B
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
&�B
'�B
'�B
(�B
)�B
+B
+B
,B
,B
,B
-B
.B
.B
/B
/B
/B
0!B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
33B
49B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
;dB
;dB
;dB
;dB
;dB
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
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
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
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
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
R�B
R�B
R�B
R�B
S�B
T�B
T�B
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
W
B
XB
XB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
]/B
]/B
]/B
^5B
^5B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BȴBȴBɺB��BɺBȴBȴBɺB��B��B��B�"BچB��B�B�B�B��B#B�B�B<B
�B1B�B �B�*B��B��B��B�2BżB�B�dB��B�B��B��B|�Bu%BkkBcnBU�BT�BT�BPbBUBO�BB�B9�B5ZB$�B�B��B��B�|B�B�`B�fB�B��B��B�B��B��B�rB�9B�OBzBshBk�B`BQhBG_B>wB4�B*KB$B�B�B
�B
�qB
�bB
҉B
�B
�mB
��B
�aB
�?B
��B
z^B
f�B
]�B
S�B
J	B
5B
(�B
�B
-B	�B	�TB	�'B	�B	��B	��B	��B	�B	�;B	�B	��B	�B	�B	�+B	�|B	��B	��B	�MB	�DB	�tB	�mB	�9B	��B	��B	��B	~�B	|�B	y>B	w2B	v�B	r|B	p!B	o5B	mCB	j0B	g�B	cnB	ZkB	M6B	G�B	:DB	5ZB	1�B	2|B	2|B	1vB	0oB	/�B	-�B	)yB	$�B	=B	}B	+B	uB��B��B�nB�B�OB�B�QB�DB�fB�nB��B��B�eB�SB҉BϑB��B��B�}B�<B�0B��B��B��B��B�B��B��B�}B��B�qB��B�B��B��B��B��B��B�B��B��B��B�B�dB��B�7B�9B�"B��B�B�AB.B{�By	BxBxRBw�Bv�Bu�Bt�Bs�Br�BraBqBpUBo�BnIBm�BkkBiyBffBc:Ba�Ba�B`�B`�B_�B^�B^�B\�B\CB[	BYBV�BU�BT�BS�BS�BR�BRBQ BO�BN�BK�BJ�BIBHfBE�BEBDBDMBC�BA�BAoB@�B=�B<�B;�B<B;dB:^B8�B8RB72B6�B5�B4�B4�B4�B4B4B3�B3B33B2�B3B1�B2-B2GB1�B0B/�B/5B/B/iB/�B/iB/�B/�B.�B0oB0oB0�B0�B0�B/iB/�B/5B0oB0�B.�B1AB33B4�B6�B7�B9	B:�B:�B:�B:�B:�B;B;B<�B=BBBCGBC�BB�BCGBB[B@�B@�BA BC�BC�BD�BD�BE�BF�BG�BH�BJ#BJ#BJXBJXBJ#BJ�BI�BH�BK�BQ�BZkB]dB_VB`vBb�Bd�Be�Be�Be�Bf�Bf�Bh
BiDBl"Bm�Bm�Bm�Bm�Bm�Bo�BqBtBtTBuZBvzBv�B{B~(B�BB�B��B��B��B��B�~B��B��B��B�B�[B�?B�WB�B�B�B�B�&B�mB�KB�"B�]B�IB�5B�5B�OB�5B�;B�UB�oB��B�%B�B��B��B��B�B�#B�)B�B�BB�vB�@B�FB�SB�EB�1B�kB�WB�CB�]B�xBݲB��B�B�B�B��B��B�B��B��B��B��B��B�B��B��B�B�6B�wB	AB	SB	YB	_B	�B	
�B	B	�B	�B	�B	B	 �B	# B	#TB	(�B	-CB	.IB	/5B	/5B	0UB	1[B	2|B	7fB	8�B	<�B	>�B	>�B	?�B	A�B	C�B	F�B	KB	NB	M�B	N�B	P.B	Q4B	RoB	T,B	UMB	XEB	YeB	Z7B	[WB	\CB	\]B	\CB	\]B	^jB	_pB	_�B	d�B	g�B	j�B	m�B	m�B	m�B	m�B	p�B	q�B	q�B	s�B	s�B	t�B	u�B	xB	y	B	y	B	y�B	y�B	z�B	|B	B	�B	�B	� B	�AB	�GB	�-B	�-B	�-B	�GB	�GB	�3B	�3B	�mB	�lB	�=B	�XB	�=B	�rB	�^B	�dB	�pB	��B	��B	��B	�[B	�)B	��B	�B	�@B	�&B	�`B	��B	�cB	�AB	�aB	�MB	�nB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	�?B	�B	�B	ЗB	�4B	��B	�2B	�SB	�KB	�CB	�]B	�xB	�dB	�jB	ބB	޸B	�B	�B	�B	�zB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�	B	�	B	�B	�*B	�B	�B	�"B	�B	�(B	�BB	�wB
 �B
MB
SB
9B
SB
YB
EB
EB
+B
KB
1B
KB
1B
KB
fB
KB
	RB
	RB
	RB
	lB

XB

XB

�B
�B
�B
�B
}B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
 �B
 �B
!B
"B
# B
$B
$B
$�B
&B
&B
%�B
'B
'B
&�B
'B
'B
(
B
(>B
)DB
*0B
+B
+6B
,=B
,"B
,WB
-CB
.IB
./B
/OB
/OB
/5B
0;B
/OB
/5B
/OB
0oB
0oB
1[B
2aB
3hB
3hB
3�B
4�B
5�B
6zB
6�B
7�B
8RB
8RB
8�B
9rB
9XB
9rB
9XB
9XB
9rB
9rB
9XB
9rB
9�B
9�B
9�B
;dB
;B
;B
;�B
;B
<�B
=qB
=�B
=�B
=qB
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
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
MB
NB
NB
N�B
N�B
N�B
OB
O�B
O�B
O�B
PB
QB
Q B
Q�B
RB
RB
R B
R B
RB
SB
SB
SB
S&B
T,B
U2B
U2B
UB
UB
VB
W$B
W?B
W$B
W?B
VB
W$B
W$B
W
B
W
B
W?B
W$B
X+B
XEB
Y1B
Y1B
Y1B
YKB
YeB
Z7B
[=B
[=B
[=B
[WB
[WB
]dB
]IB
]IB
^5B
^5B
^5B
^5B
^5B
^jB
^�B
_pB
`\B
`vB
`\B
`\B
abB
a|B
a|B
abB
bNB
b�B
bhB
bhB
cnB
c�B
c�B
dtB
dtB
dtB
dtB
dtB
e�B
e�B
e�B
e�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
hsB
h�B
h�B
h�B
iyB
iyB
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
o�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u�B
v�B
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�}<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710010033162017100100331620171001003316202211182131542022111821315420221118213154201804031937232018040319372320180403193723  JA  ARFMdecpA19c                                                                20170921033512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170920183550  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170920183552  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170920183552  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170920183553  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170920183553  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170920183553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170920183553  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170920183553  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170920183553                      G�O�G�O�G�O�                JA  ARUP                                                                        20170920185714                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170924154009  CV  JULD            G�O�G�O�F�=                JM  ARGQJMQC2.0                                                                 20170924154009  CV  JULD_LOCATION   G�O�G�O�F�=,                JM  ARGQJMQC2.0                                                                 20170924154009  CV  LATITUDE        G�O�G�O�Aݮ                JM  ARCAJMQC2.0                                                                 20170930153316  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170930153316  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103723  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123154  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                