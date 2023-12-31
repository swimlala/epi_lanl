CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-07-12T15:36:20Z creation;2017-07-12T15:36:23Z conversion to V3.1;2019-12-18T07:29:34Z update;2022-11-21T05:32:19Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170712153620  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA  I1_0397_106                     2C  DdD�NAVIS_A                         0397                            ARGO 011514                     863 @� �[g 1   @�!hK� @;�Dg8~�dD�@��41   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@�Q�@��A (�A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�{A�G�A�z�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds��Dtz=Dt�=Duz=Du�=Dvz=Dv�=Dwz=Dw�=Dxz=Dx�=Dyz=Dy�=Dzz=Dz�=D{z=D{�=D|z=D|�=D}z=D}�=D~z=D~�=Dz=D�=D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D½D��D�=D�}DýD��D�=D�}DĽD��D�=D�}DŽD��D�=D�}DƽD��D�=D�}DǽD��D�=D�}DȽD��D�=D�}DɽD��D�=D�}DʽD��D�=D�}D˽D��D�=D�}D̽D��D�=D�}DͽD��D�=D�}DνD��D�=D�}DϽD��D�=D�}DнD��D�=D�}DѽD��D�=D�}DҽD��D�=D�}DӽD��D�=D�}DԽD��D�=D�}DսD��D�=D�}DֽD��D�=D�}D׽D��D�=D�}DؽD��D�=D�}DٽD��D�=D�}DڽD��D�=D�}D۽D��D�=D�}D��RD��D�=D�}DݽD��D�=D�}D޽D��D�=D�}D߽D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D�D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D�}D��D��D�=D��RD���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?A�E�A�I�A�C�A�7LA��AΉ7Ạ�A�t�Aʣ�A��A�bA�(�A�v�A���A�XA�bA�1'A��hA��A��7A�%A�1A���A�p�A�hsA��RA�I�A��A���A�ZA�K�A��A�5?A��
A��A��!A�A�JA�7LA���A���A��PA�K�A�
=A��A�ƨA�XA��^A�;dA�A��A���A��FA��A�
=A��uA��mA�1A��A�1'A�oA�ƨA�hA}��A|v�A{�mAz��Ay|�Ax��Ax�Ax �Awt�Av�Au��As��Ar��Ar�Aq�hAoS�An�!An�DAn �Am��Al^5Ak��Akl�AkG�Aj5?Ai
=Agp�Ae�-Ad(�Ac&�Aa��Aa�A`ȴA`�A^��A]\)A\ȴA\�9A\E�A[hsAZ��AYƨAX�AW��AUG�AT�\ATM�AT1AS`BAR��AQ�TAP�yAP1AO
=AN��AN�AM�AL9XAJ��AI�^AH��AG�AF�AE�AE�AD�yADffAD�AC��AB��ABI�AB9XAB$�AB  AA��AAK�A@�jA@JA?�wA?�PA>�!A=dZA=;dA<�yA<{A;�#A;?}A:�A9�PA7�mA6bNA5x�A4�A4ZA3��A3x�A2��A21A1��A1�-A1hsA1K�A1C�A1"�A1%A0�`A0^5A/��A/\)A.�jA-�A-oA+�A*��A*{A)��A)?}A(�A(��A(�\A'�
A%�
A%l�A$�A#O�A"�9A!ƨA!oA �!A r�A bNA VA �A�TA�-A��A��A��Az�A��A��AA�A1A�A�A�A�A{A+A��AC�A��A�\A`BA �A�AbNA�7AS�A�A��A	�TA�A&�A�9AȴA�hAx�@�33@���@���@�V@�Z@��@�Ĝ@�b@�!@�J@�hs@�9@�bN@� �@�C�@�!@�@�O�@���@�1'@�"�@蛦@畁@�v�@��@�%@�1'@�P@��@��@�n�@ݙ�@݁@�X@�j@ڸR@��`@׮@׍P@�+@�V@�$�@ղ-@���@� �@���@�+@�{@��m@���@�Z@��
@��@�ff@�J@�x�@ȼj@���@�
=@���@ě�@��@�S�@�5?@�&�@��@�Q�@�1@��;@�@���@�bN@��h@�r�@���@�K�@���@��\@�M�@��@�Ĝ@�bN@�(�@���@��@���@��T@���@�1'@��@��@�ff@��@�x�@�V@���@���@�t�@��@�=q@���@�j@�\)@�=q@�x�@�1@�S�@���@�V@�&�@��w@�^5@��^@�hs@�&�@���@��`@��9@�9X@�b@��
@��P@�
=@�J@��@�G�@�V@��/@��j@�j@���@��y@�`B@��@���@���@�j@�ƨ@��@�n�@�E�@�@���@�x�@���@��@� �@�|�@���@���@�v�@���@���@�`B@��/@�1@��@��@��@��m@��;@���@���@�ƨ@�ƨ@��F@�"�@�~�@��`@�Z@�A�@� �@��@��m@��w@��F@�ƨ@�ƨ@��F@��F@���@�ƨ@�ƨ@��@��P@�dZ@�K�@�+@��@�
=@��R@�ff@�E�@�{@���@�`B@�7L@���@�(�@~�@~��@~{@}�@}��@}��@}�h@}�@}`B@}V@|�@{��@z��@z�!@zM�@y�^@yX@y&�@x��@x��@x�@xb@wl�@v�+@u��@u��@u�@t�D@s��@so@r�@r��@rn�@q��@q7L@q&�@p�`@pĜ@pr�@p �@o�@oK�@o�@n�R@nV@m�@m�@l��@lj@k��@k"�@j~�@jJ@i�@i��@i��@h�u@g|�@f�R@fff@f5?@f{@f@e`B@d��@dI�@c�
@cS�@b��@b�!@bM�@a��@a�@a�#@a��@a��@a�#@a�#@a�@a��@a�7@ax�@aX@`��@`�@`A�@` �@_�;@_��@_�P@_K�@_+@^�R@^��@^�+@^�y@_|�@_�@`A�@`1'@`1'@`b@`r�@a�@a%@a�@b~�@a��@ax�@a�@`�9@_+@]��@\�j@Z�H@Y�^@Y%@X�@Xb@W��@W�@W�P@Wl�@V��@U�@UV@Tz�@T9X@T�@T�@T(�@T9X@T9X@T�@T1@S��@S�F@S�@St�@SC�@R�@R�!@R�\@Rn�@Rn�@R^5@R^5@RM�@RM�@RM�@R=q@Q��@Qx�@QX@PA�@O�w@O��@O�P@O\)@O
=@N�@N��@Nv�@N5?@M�@M`B@L��@LI�@L�@L�@L1@KC�@J~�@JM�@J=q@J�@I��@Ix�@I%@HĜ@HbN@G��@G
=@FV@F5?@F@E��@E��@E?}@D�j@D(�@CC�@A�7@A&�@A%@@��@@��@@��@@�9@?�P@?;d@>��@>V@>E�@>$�@>{@=�@=/@<�j@<�D@<z�@<j@<Z@<Z@;�
@;�F@;ƨ@;�@;S�@;o@:~�@9��@9G�@8��@8r�@81'@7�;@7�P@7l�@7;d@7;d@7;d@6�@6�R@6��@6��@6ff@65?@6{@6@5�@5@5p�@5V@4��@4z�@4(�@3�m@3��@3S�@333@3@2�H@2��@2�@1��@1hs@1hs@1X@1G�@1&�@0��@0Ĝ@0�@0r�@0Q�@0A�@0A�@0 �@/��@/;d@/
=@.ȴ@.��@.��@.ff@.$�@-�@-��@-@-�-@-�h@,�@,j@,1@+�m@+�@+C�@+o@+@*�H@*�!@*�!@*��@*�\@*=q@*-@*�@)��@)�#@)��@)��@)��@)hs@(��@(Ĝ@(��@(�u@(Q�@'�w@'
=@&v�@&��@&�+@&ff@%��@%�-@%p�@$�@$�@$�D@$z�@$Z@$�@#ƨ@#��@#t�@#o@"�\@"^5@"=q@"�@!�@!X@!�@ ��@ �`@ Ĝ@ �9@ ��@ ��@ ��@ �u@ �@ Q�@ 1'@   @�P@ȴ@�@�y@��@5?@��@��@�@V@�j@��@z�@I�@(�@��@�m@�
@�
@��@�@�@dZ@"�@o@��@�\@~�@~�@~�@�\@�\@�\@�\@n�@M�@=q@-@�@��@hs@X@�@�`@��@Ĝ@��@r�@  @��@��@��@�w@�@�@ȴ@��@�+@ff@E�@5?@5?@{@��@��@�h@�@��@j@(�@�
@��@�@��@��@~�@-@J@�@�^@X@��@Ĝ@Ĝ@�9@�9@�u@1'@�@��@��@|�@K�@�@��@�@ȴ@�+@E�@�-@�@?}@�@V@�@�/@��@z�@j@Z@I�@(�@�@(�@Z@z�@Z@ƨ@��@�@�@t�@dZ@dZ@S�@33@"�@"�@"�@"�@"�@@
�H@
��@
��@
^5@
=q@	��@	hs@	hs@	G�@	&�@	&�@	�@�`@�9@�@Q�@ �@  @�@�P@l�@�@
=@��@ȴ@�R@�R@��@�+@�+@ff@ff@E�@E�@5?@5?@5?@5?@@�T@�T@@�-@�h@?}@�@��@��@�D@Z@I�@(�@��@ƨ@��@��@�@dZ@S�@C�@"�@@��@��@M�@-@�@��@��@��@�@�#@��@��@��@x�@hs@7L@�@%@ ��@ ��@ �`@ �`@ ��@ ��@ ��@ ��@ �9@ ��@ �@ bN@ bN@ bN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?A�E�A�I�A�C�A�7LA��AΉ7Ạ�A�t�Aʣ�A��A�bA�(�A�v�A���A�XA�bA�1'A��hA��A��7A�%A�1A���A�p�A�hsA��RA�I�A��A���A�ZA�K�A��A�5?A��
A��A��!A�A�JA�7LA���A���A��PA�K�A�
=A��A�ƨA�XA��^A�;dA�A��A���A��FA��A�
=A��uA��mA�1A��A�1'A�oA�ƨA�hA}��A|v�A{�mAz��Ay|�Ax��Ax�Ax �Awt�Av�Au��As��Ar��Ar�Aq�hAoS�An�!An�DAn �Am��Al^5Ak��Akl�AkG�Aj5?Ai
=Agp�Ae�-Ad(�Ac&�Aa��Aa�A`ȴA`�A^��A]\)A\ȴA\�9A\E�A[hsAZ��AYƨAX�AW��AUG�AT�\ATM�AT1AS`BAR��AQ�TAP�yAP1AO
=AN��AN�AM�AL9XAJ��AI�^AH��AG�AF�AE�AE�AD�yADffAD�AC��AB��ABI�AB9XAB$�AB  AA��AAK�A@�jA@JA?�wA?�PA>�!A=dZA=;dA<�yA<{A;�#A;?}A:�A9�PA7�mA6bNA5x�A4�A4ZA3��A3x�A2��A21A1��A1�-A1hsA1K�A1C�A1"�A1%A0�`A0^5A/��A/\)A.�jA-�A-oA+�A*��A*{A)��A)?}A(�A(��A(�\A'�
A%�
A%l�A$�A#O�A"�9A!ƨA!oA �!A r�A bNA VA �A�TA�-A��A��A��Az�A��A��AA�A1A�A�A�A�A{A+A��AC�A��A�\A`BA �A�AbNA�7AS�A�A��A	�TA�A&�A�9AȴA�hAx�@�33@���@���@�V@�Z@��@�Ĝ@�b@�!@�J@�hs@�9@�bN@� �@�C�@�!@�@�O�@���@�1'@�"�@蛦@畁@�v�@��@�%@�1'@�P@��@��@�n�@ݙ�@݁@�X@�j@ڸR@��`@׮@׍P@�+@�V@�$�@ղ-@���@� �@���@�+@�{@��m@���@�Z@��
@��@�ff@�J@�x�@ȼj@���@�
=@���@ě�@��@�S�@�5?@�&�@��@�Q�@�1@��;@�@���@�bN@��h@�r�@���@�K�@���@��\@�M�@��@�Ĝ@�bN@�(�@���@��@���@��T@���@�1'@��@��@�ff@��@�x�@�V@���@���@�t�@��@�=q@���@�j@�\)@�=q@�x�@�1@�S�@���@�V@�&�@��w@�^5@��^@�hs@�&�@���@��`@��9@�9X@�b@��
@��P@�
=@�J@��@�G�@�V@��/@��j@�j@���@��y@�`B@��@���@���@�j@�ƨ@��@�n�@�E�@�@���@�x�@���@��@� �@�|�@���@���@�v�@���@���@�`B@��/@�1@��@��@��@��m@��;@���@���@�ƨ@�ƨ@��F@�"�@�~�@��`@�Z@�A�@� �@��@��m@��w@��F@�ƨ@�ƨ@��F@��F@���@�ƨ@�ƨ@��@��P@�dZ@�K�@�+@��@�
=@��R@�ff@�E�@�{@���@�`B@�7L@���@�(�@~�@~��@~{@}�@}��@}��@}�h@}�@}`B@}V@|�@{��@z��@z�!@zM�@y�^@yX@y&�@x��@x��@x�@xb@wl�@v�+@u��@u��@u�@t�D@s��@so@r�@r��@rn�@q��@q7L@q&�@p�`@pĜ@pr�@p �@o�@oK�@o�@n�R@nV@m�@m�@l��@lj@k��@k"�@j~�@jJ@i�@i��@i��@h�u@g|�@f�R@fff@f5?@f{@f@e`B@d��@dI�@c�
@cS�@b��@b�!@bM�@a��@a�@a�#@a��@a��@a�#@a�#@a�@a��@a�7@ax�@aX@`��@`�@`A�@` �@_�;@_��@_�P@_K�@_+@^�R@^��@^�+@^�y@_|�@_�@`A�@`1'@`1'@`b@`r�@a�@a%@a�@b~�@a��@ax�@a�@`�9@_+@]��@\�j@Z�H@Y�^@Y%@X�@Xb@W��@W�@W�P@Wl�@V��@U�@UV@Tz�@T9X@T�@T�@T(�@T9X@T9X@T�@T1@S��@S�F@S�@St�@SC�@R�@R�!@R�\@Rn�@Rn�@R^5@R^5@RM�@RM�@RM�@R=q@Q��@Qx�@QX@PA�@O�w@O��@O�P@O\)@O
=@N�@N��@Nv�@N5?@M�@M`B@L��@LI�@L�@L�@L1@KC�@J~�@JM�@J=q@J�@I��@Ix�@I%@HĜ@HbN@G��@G
=@FV@F5?@F@E��@E��@E?}@D�j@D(�@CC�@A�7@A&�@A%@@��@@��@@��@@�9@?�P@?;d@>��@>V@>E�@>$�@>{@=�@=/@<�j@<�D@<z�@<j@<Z@<Z@;�
@;�F@;ƨ@;�@;S�@;o@:~�@9��@9G�@8��@8r�@81'@7�;@7�P@7l�@7;d@7;d@7;d@6�@6�R@6��@6��@6ff@65?@6{@6@5�@5@5p�@5V@4��@4z�@4(�@3�m@3��@3S�@333@3@2�H@2��@2�@1��@1hs@1hs@1X@1G�@1&�@0��@0Ĝ@0�@0r�@0Q�@0A�@0A�@0 �@/��@/;d@/
=@.ȴ@.��@.��@.ff@.$�@-�@-��@-@-�-@-�h@,�@,j@,1@+�m@+�@+C�@+o@+@*�H@*�!@*�!@*��@*�\@*=q@*-@*�@)��@)�#@)��@)��@)��@)hs@(��@(Ĝ@(��@(�u@(Q�@'�w@'
=@&v�@&��@&�+@&ff@%��@%�-@%p�@$�@$�@$�D@$z�@$Z@$�@#ƨ@#��@#t�@#o@"�\@"^5@"=q@"�@!�@!X@!�@ ��@ �`@ Ĝ@ �9@ ��@ ��@ ��@ �u@ �@ Q�@ 1'@   @�P@ȴ@�@�y@��@5?@��@��@�@V@�j@��@z�@I�@(�@��@�m@�
@�
@��@�@�@dZ@"�@o@��@�\@~�@~�@~�@�\@�\@�\@�\@n�@M�@=q@-@�@��@hs@X@�@�`@��@Ĝ@��@r�@  @��@��@��@�w@�@�@ȴ@��@�+@ff@E�@5?@5?@{@��@��@�h@�@��@j@(�@�
@��@�@��@��@~�@-@J@�@�^@X@��@Ĝ@Ĝ@�9@�9@�u@1'@�@��@��@|�@K�@�@��@�@ȴ@�+@E�@�-@�@?}@�@V@�@�/@��@z�@j@Z@I�@(�@�@(�@Z@z�@Z@ƨ@��@�@�@t�@dZ@dZ@S�@33@"�@"�@"�@"�@"�@@
�H@
��@
��@
^5@
=q@	��@	hs@	hs@	G�@	&�@	&�@	�@�`@�9@�@Q�@ �@  @�@�P@l�@�@
=@��@ȴ@�R@�R@��@�+@�+@ff@ff@E�@E�@5?@5?@5?@5?@@�T@�T@@�-@�h@?}@�@��@��@�D@Z@I�@(�@��@ƨ@��@��@�@dZ@S�@C�@"�@@��@��@M�@-@�@��@��@��@�@�#@��@��@��@x�@hs@7L@�@%@ ��@ ��@ �`@ �`@ ��@ ��@ ��@ ��@ �9@ ��@ �@ bN@ bN@ bN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111yB'�B'�B'�B&�B#�B1B��B��B��B��B�B�B�B�fB��B��B~�Bl�BgmB]/BR�BK�BI�BG�BC�B?}B7LB �BoB  B��B�B�yB�;B�#B��BǮBƨBB�jB�3B��B��B��B��B�DB�B}�Bu�BffBXBR�BF�B;dB&�BVB
�B
�HB
��B
ĜB
B
�dB
��B
��B
�uB
�VB
�%B
}�B
w�B
v�B
r�B
l�B
gmB
]/B
P�B
F�B
?}B
8RB
+B
+B
+B
(�B
$�B
�B
�B
�B
{B
PB
%B	��B	�B	�mB	�BB	�B	��B	��B	��B	��B	�dB	�LB	�FB	�-B	�B	��B	��B	��B	��B	�7B	�B	�B	~�B	y�B	u�B	p�B	k�B	ffB	aHB	^5B	]/B	XB	O�B	G�B	B�B	>wB	9XB	49B	1'B	.B	,B	)�B	'�B	%�B	!�B	�B	�B	�B	�B	�B	�B	�B	uB	oB	bB	JB	1B	%B	B	B	  B��B��B��B�B�sB�ZB�NB�BB�5B�)B�B�
B�B��B��B��B��B��B��B��B��B��B��BȴBŢB��B�qB�^B�LB�FB�9B�9B�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�VB�%B�B�B~�B}�Bz�Bw�Bu�Br�Bo�Bl�BiyBgmBe`BcTB`BB]/BZBXBVBT�BQ�BM�BI�BD�BA�B>wB;dB6FB2-B/B-B+B'�B&�B$�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBhBoBhBhBbB\B\B\B\B\B\B\BVBVBPBVBPBJBDBDBPBPBJBJBPBJBJBJBJBJBPBPBPBPBVBVB\B\B\B\B\B\BhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B"�B'�B/B0!B2-B49B5?B7LB7LB;dB?}B@�BC�BE�BH�BG�BJ�BL�BO�BO�BS�BZBbNBdZBffBhsBiyBjBk�Bn�Bo�Bp�Br�Bt�Bz�B~�B� B�B�B�B�B�+B�DB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�?B�RB�XB�wB��BBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�BB�HB�NB�NB�TB�ZB�ZB�`B�`B�`B�`B�fB�fB�fB�mB�sB�yB�B�B�B�B�B��B��B��B��B	B	B	B		7B	\B	bB	oB	oB	uB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	%�B	%�B	'�B	(�B	.B	1'B	2-B	5?B	7LB	;dB	>wB	?}B	?}B	A�B	C�B	E�B	E�B	F�B	G�B	H�B	I�B	J�B	L�B	L�B	M�B	N�B	Q�B	S�B	S�B	VB	ZB	\)B	^5B	`BB	aHB	aHB	bNB	e`B	hsB	jB	l�B	l�B	l�B	l�B	o�B	s�B	u�B	x�B	|�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�=B	�=B	�DB	�PB	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�RB	�dB	��B	��B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
	7B
DB
DB
VB
VB
\B
bB
bB
bB
bB
hB
bB
hB
bB
bB
hB
bB
hB
hB
oB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
+B
+B
,B
,B
,B
-B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
5?B
5?B
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
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
>wB
>wB
>wB
@�B
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
D�B
E�B
E�B
F�B
F�B
F�B
H�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
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
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
cTB
dZB
dZB
cTB
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
e`B
dZB
dZB
e`B
e`B
ffB
ffB
ffB
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
gmB
hsB
hsB
hsB
hsB
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
m�B
m�B
m�B
n�B
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
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111yB(
B($B(�B(�B(�B�B�jB��B��B��B��B��B�!B�AB�;B��B��Bp�BlB`BBT�BL�BJ�BI�BE�BBB=�B(�BB[B�B��B�qB�BޞB�gB��BȚBāB�cB��B��B��B�B�$B�~B��B�BxlBh�BYKBUBH�B>BB*B�B
�B
�B
�uB
�SB
ðB
��B
�"B
�;B
�{B
��B
�_B
~�B
x8B
w�B
s�B
m�B
iDB
_B
RoB
G�B
@�B
:xB
+�B
+kB
+�B
)�B
&2B
�B
B
$B
�B
�B
1B	�(B	�|B	��B	��B	��B	ԯB	�B	̈́B	��B	�B	��B	��B	�hB	�)B	�eB	�LB	��B	��B	�	B	�{B	��B	� B	z�B	v�B	q�B	l�B	g�B	a�B	^�B	^OB	ZB	Q�B	H�B	C�B	?�B	:�B	5%B	1�B	/ B	,�B	*B	(�B	&�B	"hB	B	B	B	B	xB	�B	YB	B	&B	�B	�B	�B	�B	B	�B	 �B��B��B��B�oB�B�FB� B��B�B�IB��B�YBևB�gB�,B�FB�FB�@B�uBҽB��B̈́B��B��B��B�B��B�JB�B�B��B��B�B��B�UB��B�B��B��B��B��B�+B��B��B��B�B��B��B��B��B��B�4B��B��B��B�BHB|Bx�Bv�BtBp�Bm�BjBh>BfBeBa�B^�B[=BYBV�BV9BS�BPbBK�BFBB�BAB=qB9$B4TB/�B.�B-CB(�B(XB%�B$�B#�B"hB"hB!HB 'B BBpBVBOB/B/BCB�BBKBEB$B
BBBB�B@B�B�B�B4B}B�BB�B�B�B�B�B�B�B�B�BPB�B�B�B�B�B�B�B�B�BBB6BB�B�B"B�B�B�B�B�BHB}B�B&BSB$B�B�B�BBQBIB5BBVBVB \B vB!�B#�B#�B$�B)yB/�B0�B2�B4�B5�B7�B7�B<B@BA�BD�BF�BIlBH�BK^BMjBP}BP�BUB[#Bb�Bd�Bf�Bh�Bi�Bj�Bk�Bn�BpBqBsMButB{JB.B�OB�;B�[B��B��B��B�0B��B��B��B��B�7B�IB�-B� B�@B�FB�`B��B�QB�}B��B��B��B��B��B��B��B�B�1B�B�B��B�B�B� B� B�B�B�:B҉BԯB��BݘB�\B�|B�B�B�nB�tB�tB�B�zB�`B�zB�B�B�B�B��B�B�B�B��B� B��B��B�$B�JB�HB	UB	[B	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	
B		B	�B	B	�B	!�B	#�B	%,B	&B	%�B	&B	(>B	)_B	.cB	1[B	2�B	5�B	7�B	;�B	>�B	?�B	?�B	A�B	C�B	E�B	E�B	F�B	G�B	H�B	I�B	KB	L�B	MB	N"B	O(B	R:B	TB	TFB	VmB	ZkB	\xB	^jB	`\B	abB	a|B	b�B	e�B	h�B	j�B	l�B	l�B	l�B	l�B	o�B	s�B	u�B	y$B	}"B	� B	�;B	�3B	�?B	�_B	�KB	�7B	�=B	�=B	�XB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�GB	�MB	�TB	�B	�0B	��B	�UB	żB	�	B	�B	�"B	�"B	�pB	�pB	�PB	�jB	�)B	�B	��B	��B	��B	��B	��B	��B	�#B	�<B	�.B	�B	�B	� B	��B	�B	��B	�B	�?B	�+B	�EB	�+B	�1B	�QB	�WB	�dB	�pB	�vB	�vB	�BB	�vB	�bB	�bB	�NB	�hB	�hB	�nB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�2B	�B	��B	��B	��B	�B	�$B	�*B	�B	�"B	�<B	�HB
 OB
'B
'B
'B
GB
MB
tB
	�B
�B
�B
pB
pB
\B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"B
# B
$B
#�B
$�B
%B
'B
(
B
($B
($B
)B
)*B
)DB
)DB
+6B
+B
,B
,=B
,"B
-CB
./B
/5B
0;B
0UB
0;B
1'B
1[B
1vB
1[B
3MB
4nB
4nB
4nB
5tB
5tB
5ZB
5ZB
5ZB
5tB
5ZB
4�B
5ZB
5ZB
5ZB
5ZB
5ZB
6zB
6zB
7�B
7fB
7LB
7fB
7fB
7fB
7fB
7fB
7�B
7�B
8lB
8lB
9rB
:xB
:�B
:xB
:xB
:xB
:�B
:�B
:�B
;�B
<jB
>�B
>�B
>�B
@�B
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
D�B
E�B
E�B
F�B
F�B
F�B
H�B
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
LB
K�B
L�B
L�B
MB
MB
MB
MB
MB
M�B
OB
OB
N�B
PB
PB
O�B
O�B
O�B
PB
Q B
P�B
Q B
Q B
R B
RB
Q�B
RB
Q�B
RB
RB
RB
Q�B
Q�B
RB
RB
Q�B
R B
RB
RB
SB
SB
S&B
TB
TB
T�B
UB
UB
V9B
V9B
VB
VB
VB
V9B
V9B
W$B
W?B
W$B
X+B
X+B
X+B
XB
XEB
X+B
XEB
XEB
XEB
X+B
Y1B
Y1B
Y1B
Y1B
YKB
ZQB
Z7B
ZQB
Z7B
[WB
\]B
\]B
\xB
]dB
]/B
^OB
^5B
^OB
^jB
^OB
^OB
^jB
_pB
_pB
_VB
_VB
_VB
_VB
_pB
_VB
`vB
`vB
`\B
`\B
`\B
`\B
abB
abB
a|B
abB
aHB
aHB
abB
abB
bNB
bhB
cnB
cnB
d�B
d�B
cnB
cTB
cTB
cnB
cTB
dtB
dtB
dtB
e`B
e`B
ezB
ezB
dtB
d�B
e�B
ezB
f�B
f�B
f�B
e�B
ezB
ffB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
iyB
i�B
j�B
jB
j�B
jB
jB
j�B
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
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
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
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-��<�o<[��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.09(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707230033102017072300331020170723003310202211182131052022111821310520221118213105201804031936282018040319362820180403193628  JA  ARFMdecpA19c                                                                20170713003515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170712153620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170712153621  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170712153622  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170712153622  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170712153622  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170712153622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170712153622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170712153623  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170712153623                      G�O�G�O�G�O�                JA  ARUP                                                                        20170712161002                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170712153325  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170722153310  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170722153310  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103628  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123105  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                