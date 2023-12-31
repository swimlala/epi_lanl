CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-20T09:00:42Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20230420090042  20230420090042  5906096 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  7902                            2B  A   NAVIS_A                         1010                            170425                          863 @�%'�1   @�%���@)�vȴ9X�dq���-�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�33B���B���B�  B�  B�33B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�3D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��R@��RA\)A;\)A[\)A{\)A��A��A��A��AͮAݮA��A��B�
B�
B�
B�
B&�
B.�
B6�
B>�
BF�
BN�
BV�
B^�
Bf�
Bn�
Bv�
B~�
B�k�B�k�B�k�B�k�B���B���B���B�8RB�8RB�k�B�k�B���B�k�B���B�8RB�8RB�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE��DFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt�qDumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqDz�qD{mqD{�qD|mqD|�qD}mqD}�qD~mqD~�qDmqD�qD�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D¶�D���D�6�D�v�Dö�D���D�6�D�v�DĶ�D���D�6�D�v�DŶ�D���D�6�D�v�Dƶ�D���D�6�D�v�DǶ�D���D�6�D�v�Dȶ�D���D�6�D�v�Dɶ�D���D�6�D�v�Dʶ�D���D�6�D�v�D˶�D���D�6�D�v�D̶�D���D�6�D�v�DͶ�D���D�6�D�v�Dζ�D���D�6�D�v�D϶�D���D�6�D�v�Dж�D���D�6�D�v�DѶ�D���D�6�D�v�DҶ�D���D�6�D�v�DӶ�D���D�6�D�v�DԶ�D���D�6�D�v�Dն�D���D�6�D�v�Dֶ�D���D�6�D�v�D׶�D���D�6�D�v�Dض�D���D�6�D�v�Dٶ�D���D�6�D�v�Dڶ�D���D�6�D�v�D۶�D���D�6�D�v�Dܶ�D���D�6�D�v�Dݶ�D���D�6�D�v�D޶�D���D�6�D�v�D߶�D���D�6�D�v�DමD���D�6�D�v�DᶸD���D�6�D�v�DⶸD���D�6�D�v�D㶸D���D�6�D�v�D䶸D���D�6�D�v�D嶸D���D�6�D�v�D涸D���D�6�D�v�D綸D���D�6�D�v�D趸D���D�6�D�v�D鶸D���D�6�D�v�D궸D���D�6�D�v�D붸D��D�6�D�v�D춸D���D�6�D�v�D���D���D�6�D�v�DD���D�6�D�v�DﶸD���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D��D�0R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A؉7A�JA״9Aש�Aח�Aׇ+A�x�A�p�A�jA�dZA�VA�I�A�A�A�5?A�+A�$�A�{A�
=A���A��A��A��HA���A�ȴA־wAְ!AօA�M�A�oA��A��/A���Aմ9A�x�A� �A�ƨA�?}A�33A�&�AǮA�{A���A�+A�oA�jA�dZA�9XA���A�ȴA�%A��A�VA��A���A���A�ffA�=qA�O�A�7LA�v�A��A��/A�^5A�oA�&�A��
A��wA�{A� �A�5?A�(�A��+A��A��^A��A�=qA�n�A�VA��^A��A��A}�AxAt9XArȴAo33Ah��Ad�\Aa�mA`  A\�jAV�`AR�9AO?}AL�AK��AIt�AE��AD�RACƨAAXA<��A;�mA;�A9�A8^5A6�uA4n�A1��A1&�A0~�A/��A.�A,��A,A+��A*�A*M�A)��A)oA'��A&��A%�#A%K�A#�A#+A"�A"�9A"-A!��A!oA �+A =qA�^A�hA�hA�hAS�A�yA�DA�AbNA�At�A;dA�HAn�A{A&�A�A�A��AE�A  A��A�AXA;dAVA�A-A�#A�FA��AXA�AA�A�TA��A"�A��AZA�mA��AhsA\)AS�A�A��Ap�A&�A~�A�
A��A�Ax�AdZA;dA��A�DA�A�hA�A�HA��A��A��A�A(�A�-A|�AO�A/AVA
�A
�A
Q�A	�A	p�A	;dA��A�jA5?A��AO�AA�uA1A��AXA;dA�A��A��A-A�
A�7A7LA�A�HAĜA�RA��AffA9XA��A��A`BA ��A �@���@�+@�n�@��^@��@�bN@��
@�\)@���@�^5@��-@���@�z�@���@���@�@�`B@���@�r�@�9X@�ƨ@�t�@��@�{@��T@�hs@���@�9@�@�Z@���@�l�@�;d@���@�v�@�=q@��@��#@홚@�7@�p�@�O�@��`@���@�P@�+@�E�@��@�-@�X@�z�@� �@���@�K�@�+@���@噚@�hs@�&�@�bN@㕁@�33@�n�@�M�@��@�x�@�7L@�V@�9@�Q�@�(�@���@߅@�33@ާ�@�E�@�%@�j@�Z@�b@�K�@�V@�$�@��@١�@��@���@�z�@���@�l�@�ff@ղ-@ԣ�@ӥ�@�C�@��@��@�~�@Ѻ^@с@�X@��@�%@���@�Z@�t�@��y@�~�@�{@�G�@�%@���@̓u@�1'@���@�\)@��@�~�@��@ȋD@�1@�dZ@Ƈ+@�J@��#@ź^@ũ�@�X@�%@���@ě�@�9X@�t�@���@�M�@�{@���@�/@�j@��m@��@�$�@���@�p�@��/@��9@���@��@�1@�
=@�-@��@�1'@�  @���@�t�@��@�ff@�@�p�@��`@��j@��u@�A�@��@�C�@���@�~�@�M�@�{@��@���@��^@���@���@��@�%@�V@�Ĝ@��@�ƨ@���@��@�`B@���@�r�@�1@��P@�S�@��@���@���@��+@�^5@�M�@�@�x�@�O�@��@��u@�9X@��@�+@���@�n�@�^5@��@���@�`B@�?}@���@���@��P@�C�@�
=@���@�ȴ@��R@���@�n�@��@��7@��/@�(�@�1@�dZ@�
=@���@��H@��R@�E�@���@�O�@��9@� �@�1@��;@���@�S�@���@�E�@��@���@�hs@��D@� �@�b@��@�l�@�dZ@�+@��!@�5?@�J@���@��#@��h@�?}@��j@��@�Z@�  @���@�dZ@�;d@�+@���@��\@�v�@�v�@�n�@�@��^@���@�G�@�V@�r�@�9X@� �@��;@��@�t�@�\)@�C�@�+@�
=@��y@���@��+@��@���@�x�@�`B@�G�@�?}@�7L@�&�@���@���@��P@�\)@�;d@��@��H@��R@���@�n�@�{@�`B@��@��@�bN@��@�\)@��H@�~�@�-@�J@���@�O�@�Ĝ@�j@�bN@�Z@�I�@�I�@�9X@�(�@�1@��@���@��m@�ƨ@��w@��w@��@�dZ@�C�@��@�ȴ@���@�~�@�E�@�{@�@���@��@��#@���@�hs@��@��`@��j@��9@��@��@��@��@�@�@�P@\)@+@~�@~�+@~{@}��@}`B@|��@|��@|��@|z�@|I�@{�m@{33@z��@z�\@zJ@xĜ@x  @w;d@vV@u@uO�@u?}@t�/@t��@tz�@t1@s�@s"�@r^5@q�7@q�@p��@p�@pr�@p1'@pb@o�@o|�@n�R@nV@n5?@m�@m�@m?}@l�@k��@k��@kt�@kdZ@k"�@jn�@jJ@i��@i�#@i�^@i�7@ihs@i7L@h��@h��@h  @g�P@g�@fff@e�-@eV@d�@d(�@cƨ@cS�@b�H@b�!@bn�@bJ@a�@a��@`r�@` �@_��@_K�@^�y@^��@]�@]V@\(�@[��@[��@[33@Z�!@Y7L@X1'@Xb@W�;@W�@W|�@Wl�@WK�@W�@V�@Vv�@VE�@V@U�@U�T@U��@U`B@UO�@T�j@TI�@S��@SdZ@R�!@R^5@Q��@QG�@Q�@P�u@PQ�@P �@O��@O��@N��@M�-@M�@MO�@M�@Lz�@K�m@K��@KdZ@J�H@J��@JM�@J-@I��@I�@H��@H�u@HbN@HQ�@H1'@G�@GK�@F��@E��@EV@D��@DZ@Cƨ@CC�@B��@B�!@BM�@A�@A��@Ahs@A&�@A�@A%@@Q�@?�@?��@?\)@?�@>��@>�@>ȴ@>�R@>��@>v�@>$�@=�@=�T@=�T@=�T@=��@=O�@<�/@<(�@;ƨ@;S�@;o@:��@:��@:��@:~�@9��@8��@8Q�@7�;@7��@7�w@7|�@7l�@7;d@6��@6��@6{@5O�@5V@4�/@4�j@4�@4��@4�D@4j@49X@4(�@4(�@4�@3ƨ@3dZ@333@3o@2�@2�!@2n�@2M�@2-@2J@1��@1�#@1�#@1��@1hs@1G�@1%@0Q�@/�@/|�@/+@/
=@.ȴ@.E�@-��@-�@,�/@,z�@+ƨ@+o@*�\@*-@*-@*-@*�@*�@)�@)X@)%@(��@(r�@(bN@(A�@( �@(b@'�@'��@'\)@'+@'+@'+@'�@&ȴ@&E�@&{@%�-@%p�@%`B@%V@$�@$�@$�/@$��@$�D@$I�@#�m@#dZ@#"�@"�@"n�@"-@!�@!G�@!&�@!�@!%@ ��@ �u@ A�@  �@�@�w@�@��@l�@\)@+@ȴ@��@v�@V@5?@{@��@�h@`B@�@I�@9X@9X@9X@1@�
@��@��@t�@dZ@"�@�@�H@�!@�!@��@��@��@~�@M�@�#@x�@%@�u@bN@A�@  @�w@��@l�@;d@�@�R@��@ff@@�T@@�h@p�@/@�@j@9X@9X@1@�F@��@��@C�@o@�H@��@�\@M�@-@-@J@�@x�@7L@&�@%@��@�`@Ĝ@�u@�@r�@bN@bN@b@  @  @�@�@�;@�w@�P@\)@
=@�+@V@�@�T@O�@��@��@�j@��@Z@9X@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A؉7A�JA״9Aש�Aח�Aׇ+A�x�A�p�A�jA�dZA�VA�I�A�A�A�5?A�+A�$�A�{A�
=A���A��A��A��HA���A�ȴA־wAְ!AօA�M�A�oA��A��/A���Aմ9A�x�A� �A�ƨA�?}A�33A�&�AǮA�{A���A�+A�oA�jA�dZA�9XA���A�ȴA�%A��A�VA��A���A���A�ffA�=qA�O�A�7LA�v�A��A��/A�^5A�oA�&�A��
A��wA�{A� �A�5?A�(�A��+A��A��^A��A�=qA�n�A�VA��^A��A��A}�AxAt9XArȴAo33Ah��Ad�\Aa�mA`  A\�jAV�`AR�9AO?}AL�AK��AIt�AE��AD�RACƨAAXA<��A;�mA;�A9�A8^5A6�uA4n�A1��A1&�A0~�A/��A.�A,��A,A+��A*�A*M�A)��A)oA'��A&��A%�#A%K�A#�A#+A"�A"�9A"-A!��A!oA �+A =qA�^A�hA�hA�hAS�A�yA�DA�AbNA�At�A;dA�HAn�A{A&�A�A�A��AE�A  A��A�AXA;dAVA�A-A�#A�FA��AXA�AA�A�TA��A"�A��AZA�mA��AhsA\)AS�A�A��Ap�A&�A~�A�
A��A�Ax�AdZA;dA��A�DA�A�hA�A�HA��A��A��A�A(�A�-A|�AO�A/AVA
�A
�A
Q�A	�A	p�A	;dA��A�jA5?A��AO�AA�uA1A��AXA;dA�A��A��A-A�
A�7A7LA�A�HAĜA�RA��AffA9XA��A��A`BA ��A �@���@�+@�n�@��^@��@�bN@��
@�\)@���@�^5@��-@���@�z�@���@���@�@�`B@���@�r�@�9X@�ƨ@�t�@��@�{@��T@�hs@���@�9@�@�Z@���@�l�@�;d@���@�v�@�=q@��@��#@홚@�7@�p�@�O�@��`@���@�P@�+@�E�@��@�-@�X@�z�@� �@���@�K�@�+@���@噚@�hs@�&�@�bN@㕁@�33@�n�@�M�@��@�x�@�7L@�V@�9@�Q�@�(�@���@߅@�33@ާ�@�E�@�%@�j@�Z@�b@�K�@�V@�$�@��@١�@��@���@�z�@���@�l�@�ff@ղ-@ԣ�@ӥ�@�C�@��@��@�~�@Ѻ^@с@�X@��@�%@���@�Z@�t�@��y@�~�@�{@�G�@�%@���@̓u@�1'@���@�\)@��@�~�@��@ȋD@�1@�dZ@Ƈ+@�J@��#@ź^@ũ�@�X@�%@���@ě�@�9X@�t�@���@�M�@�{@���@�/@�j@��m@��@�$�@���@�p�@��/@��9@���@��@�1@�
=@�-@��@�1'@�  @���@�t�@��@�ff@�@�p�@��`@��j@��u@�A�@��@�C�@���@�~�@�M�@�{@��@���@��^@���@���@��@�%@�V@�Ĝ@��@�ƨ@���@��@�`B@���@�r�@�1@��P@�S�@��@���@���@��+@�^5@�M�@�@�x�@�O�@��@��u@�9X@��@�+@���@�n�@�^5@��@���@�`B@�?}@���@���@��P@�C�@�
=@���@�ȴ@��R@���@�n�@��@��7@��/@�(�@�1@�dZ@�
=@���@��H@��R@�E�@���@�O�@��9@� �@�1@��;@���@�S�@���@�E�@��@���@�hs@��D@� �@�b@��@�l�@�dZ@�+@��!@�5?@�J@���@��#@��h@�?}@��j@��@�Z@�  @���@�dZ@�;d@�+@���@��\@�v�@�v�@�n�@�@��^@���@�G�@�V@�r�@�9X@� �@��;@��@�t�@�\)@�C�@�+@�
=@��y@���@��+@��@���@�x�@�`B@�G�@�?}@�7L@�&�@���@���@��P@�\)@�;d@��@��H@��R@���@�n�@�{@�`B@��@��@�bN@��@�\)@��H@�~�@�-@�J@���@�O�@�Ĝ@�j@�bN@�Z@�I�@�I�@�9X@�(�@�1@��@���@��m@�ƨ@��w@��w@��@�dZ@�C�@��@�ȴ@���@�~�@�E�@�{@�@���@��@��#@���@�hs@��@��`@��j@��9@��@��@��@��@�@�@�P@\)@+@~�@~�+@~{@}��@}`B@|��@|��@|��@|z�@|I�@{�m@{33@z��@z�\@zJ@xĜ@x  @w;d@vV@u@uO�@u?}@t�/@t��@tz�@t1@s�@s"�@r^5@q�7@q�@p��@p�@pr�@p1'@pb@o�@o|�@n�R@nV@n5?@m�@m�@m?}@l�@k��@k��@kt�@kdZ@k"�@jn�@jJ@i��@i�#@i�^@i�7@ihs@i7L@h��@h��@h  @g�P@g�@fff@e�-@eV@d�@d(�@cƨ@cS�@b�H@b�!@bn�@bJ@a�@a��@`r�@` �@_��@_K�@^�y@^��@]�@]V@\(�@[��@[��@[33@Z�!@Y7L@X1'@Xb@W�;@W�@W|�@Wl�@WK�@W�@V�@Vv�@VE�@V@U�@U�T@U��@U`B@UO�@T�j@TI�@S��@SdZ@R�!@R^5@Q��@QG�@Q�@P�u@PQ�@P �@O��@O��@N��@M�-@M�@MO�@M�@Lz�@K�m@K��@KdZ@J�H@J��@JM�@J-@I��@I�@H��@H�u@HbN@HQ�@H1'@G�@GK�@F��@E��@EV@D��@DZ@Cƨ@CC�@B��@B�!@BM�@A�@A��@Ahs@A&�@A�@A%@@Q�@?�@?��@?\)@?�@>��@>�@>ȴ@>�R@>��@>v�@>$�@=�@=�T@=�T@=�T@=��@=O�@<�/@<(�@;ƨ@;S�@;o@:��@:��@:��@:~�@9��@8��@8Q�@7�;@7��@7�w@7|�@7l�@7;d@6��@6��@6{@5O�@5V@4�/@4�j@4�@4��@4�D@4j@49X@4(�@4(�@4�@3ƨ@3dZ@333@3o@2�@2�!@2n�@2M�@2-@2J@1��@1�#@1�#@1��@1hs@1G�@1%@0Q�@/�@/|�@/+@/
=@.ȴ@.E�@-��@-�@,�/@,z�@+ƨ@+o@*�\@*-@*-@*-@*�@*�@)�@)X@)%@(��@(r�@(bN@(A�@( �@(b@'�@'��@'\)@'+@'+@'+@'�@&ȴ@&E�@&{@%�-@%p�@%`B@%V@$�@$�@$�/@$��@$�D@$I�@#�m@#dZ@#"�@"�@"n�@"-@!�@!G�@!&�@!�@!%@ ��@ �u@ A�@  �@�@�w@�@��@l�@\)@+@ȴ@��@v�@V@5?@{@��@�h@`B@�@I�@9X@9X@9X@1@�
@��@��@t�@dZ@"�@�@�H@�!@�!@��@��@��@~�@M�@�#@x�@%@�u@bN@A�@  @�w@��@l�@;d@�@�R@��@ff@@�T@@�h@p�@/@�@j@9X@9X@1@�F@��@��@C�@o@�H@��@�\@M�@-@-@J@�@x�@7L@&�@%@��@�`@Ĝ@�u@�@r�@bN@bN@b@  @  @�@�@�;@�w@�P@\)@
=@�+@V@�@�T@O�@��@��@�j@��@Z@9X@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�RB
�FB
�9B
�-B
�-B
�'B
�'B
�!B
�!B
�!B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
�B1B6FB�B��B��B�
B�`B+B0!BN�Bn�Bs�B~�B�=B�oB��B��B��B��B��B��B�{B�DBF�B�B
=B	7B��B��B��By�BgmBP�BB�B8RB.B�BB
��B
�fB
��B
�jB
�B
��B
�%B
s�B
jB
aHB
M�B
B�B
8RB
-B
 �B
�B
�B
{B
\B

=B
JB
+B
B
B	��B	�B	�)B	�5B	�fB	�ZB	�B	��B
B
B
+B
JB
�B
�B
$�B
&�B
,B
5?B
8RB
<jB
F�B
R�B
P�B
Q�B
[#B
^5B
`BB
aHB
cTB
gmB
jB
m�B
o�B
r�B
s�B
s�B
s�B
w�B
x�B
{�B
z�B
|�B
}�B
�B
�+B
�DB
�PB
�hB
�\B
�\B
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�-B
�FB
�FB
�LB
�LB
�RB
�dB
�qB
�}B
��B
�}B
��B
ÖB
ĜB
ŢB
ƨB
ǮB
ȴB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ɺB
ɺB
ȴB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ƨB
ŢB
ƨB
ĜB
ÖB
ÖB
ÖB
B
ÖB
ÖB
ÖB
B
B
B
B
B
��B
��B
��B
�}B
�qB
�jB
�jB
�dB
�dB
�^B
�^B
�^B
�XB
�^B
�XB
�XB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�RB
�RB
�XB
�XB
�RB
�RB
�RB
�LB
�FB
�FB
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�9B
�3B
�3B
�3B
�-B
�-B
�-B
�-B
�'B
�!B
�!B
�!B
�!B
�!B
�-B
�'B
�'B
�!B
�!B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�9B
�9B
�9B
�9B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
B
ÖB
B
B
ÖB
B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ŢB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ǮB
ȴB
ɺB
ɺB
ɺB
��B
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�5B
�5B
�;B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�yB
�yB
�yB
�yB
�B
�B
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
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  B  B  B  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB%BB%B%B%B+B+B1B+B1B1B1B1B1B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=BDBDBDBJBDBJBJBJBJBJBJBPBPBPBPBPBPBPBVBVBVBVBVBVBPBPBPBPBPBPBPBVBVBVBVBPBVBVBVBVB\B\B\B\B\BbBbBbBbBbBhBhB
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B
�RB
�FB
�9B
�-B
�-B
�'B
�'B
�!B
�!B
�!B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
�B1B6FB�B��B��B�
B�`B+B0!BN�Bn�Bs�B~�B�=B�oB��B��B��B��B��B��B�{B�DBF�B�B
=B	7B��B��B��By�BgmBP�BB�B8RB.B�BB
��B
�fB
��B
�jB
�B
��B
�%B
s�B
jB
aHB
M�B
B�B
8RB
-B
 �B
�B
�B
{B
\B

=B
JB
+B
B
B	��B	�B	�)B	�5B	�fB	�ZB	�B	��B
B
B
+B
JB
�B
�B
$�B
&�B
,B
5?B
8RB
<jB
F�B
R�B
P�B
Q�B
[#B
^5B
`BB
aHB
cTB
gmB
jB
m�B
o�B
r�B
s�B
s�B
s�B
w�B
x�B
{�B
z�B
|�B
}�B
�B
�+B
�DB
�PB
�hB
�\B
�\B
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�-B
�FB
�FB
�LB
�LB
�RB
�dB
�qB
�}B
��B
�}B
��B
ÖB
ĜB
ŢB
ƨB
ǮB
ȴB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ȴB
ɺB
ɺB
ȴB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ƨB
ŢB
ƨB
ĜB
ÖB
ÖB
ÖB
B
ÖB
ÖB
ÖB
B
B
B
B
B
��B
��B
��B
�}B
�qB
�jB
�jB
�dB
�dB
�^B
�^B
�^B
�XB
�^B
�XB
�XB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�RB
�RB
�XB
�XB
�RB
�RB
�RB
�LB
�FB
�FB
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�9B
�3B
�3B
�3B
�-B
�-B
�-B
�-B
�'B
�!B
�!B
�!B
�!B
�!B
�-B
�'B
�'B
�!B
�!B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�9B
�9B
�9B
�9B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
B
ÖB
B
B
ÖB
B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ŢB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ǮB
ȴB
ɺB
ɺB
ɺB
��B
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�/B
�/B
�/B
�/B
�/B
�/B
�5B
�5B
�5B
�5B
�5B
�;B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�`B
�fB
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�yB
�yB
�yB
�yB
�B
�B
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
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  B  B  B  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB%BB%B%B%B+B+B1B+B1B1B1B1B1B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=BDBDBDBJBDBJBJBJBJBJBJBPBPBPBPBPBPBPBVBVBVBVBVBVBPBPBPBPBPBPBPBVBVBVBVBPBVBVBVBVB\B\B\B\B\BbBbBbBbBbBhBhB
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                  " % * 2 Q � 6     ' 0 1 &              # + 3 3 ' "                 ! & & ' &                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       - q00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230420090042                                          AO  ARCAADJP                                                                    20230420090042    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230420090042  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230420090042  QCF$                G�O�G�O�G�O�8000            