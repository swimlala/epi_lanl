CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-24T12:01:04Z creation      
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
resolution        =���   axis      Z        `  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  O4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  bl   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �l   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230524120104  20230524120104  4903319 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      rA   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @�-��q1   @�-�I���@:�f�A��d���l�D1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         rA   A   A       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�\)@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+uD+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�RD@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�RDZhRDZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�DduDd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt�Dun�Du�Dvn�Dv�Dwn�Dw�Dxn�Dx�Dyn�Dy�Dzn�Dz�D{n�D{�D|n�D|�D}n�D}�D~n�D~�Dn�D�D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D���D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D·\D��\D�7\D�w\D÷\D��\D�7\D�w\Dķ\D��\D�7\D�w\Dŷ\D��\D�7\D�w\DƷ\D��\D�7\D�w\DǷ\D��\D�7\D�w\Dȷ\D��\D�7\D�w\Dɷ\D��\D�7\D�w\Dʷ\D��\D�7\D�w\D˷\D��\D�7\D�w\D̷\D��\D�7\D�w\Dͷ\D��\D�7\D�w\Dη\D��\D�7\D�w\DϷ\D��\D�7\D�w\Dз\D��\D�7\D�w\Dѷ\D��\D�7\D�w\Dҷ\D��\D�7\D�w\Dӷ\D��\D�7\D�w\DԷ\D��\D�7\D�w\Dշ\D��\D�7\D�w\Dַ\D��\D�7\D�w\D׷\D��\D�7\D�w\Dط\D��\D�7\D�w\Dٷ\D��\D�7\D�w\Dڷ\D��\D�7\D�w\D۷\D��\D�7\D�w\Dܷ\D��\D�7\D�w\Dݷ\D��\D�7\D�w\D޷\D��\D�7\D�w\D߷\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D���D�:�D�w\D�\D��\D�7\D�t)D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D���D�*�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��\A�/A��A���A���A��A��A�~�A�r�A�+A��A��jA�l�A�M�A�/A�  A���A���A���A��uA��hA��A�~�A�t�A�n�A�dZA�K�A�7LA�&�A��A���A��A��HA���A�C�A�O�A��A�`BA��A�O�A�(�A���A���A��A�7LA��A��A�=qA���A�=qA��`A��^A��A�VA�-A�\)A��
A�~�A�I�A�ȴA�I�A��hA�9XA��A�ZA���A���A��A���A�bNA�JA�M�A��jA�z�A�JA���A�v�A��TA��A��jA�x�A�E�A�ĜA�+A��A�;dA��A�M�A��wA��uA�t�A�jA�(�A��\A�K�A�&�A���A��\A��A�hsA�A�|�A��mA��A�;dA�n�A���A�ZA��-A���A���A��A�(�A��A�O�A���A���A�  A���A��A��A�oA��mA���A�I�A��yA�^5A�9XA��A�ƨA�(�A��TA��PA~�`A}|�A|-Ay�AvȴAu�As��Aq�Ao�-Am�FAk��Aj��Ai�FAhbAf�DAdQ�Aa�A[�
AW��AWoAV9XAU/AT��AS��AS�AS?}AQ�mAQ33AO��AM�
AK�AK+AJ��AI�mAHȴAF��AF�AE��AD��ADr�AC/ABr�AA�^A?�A=G�A;hsA:��A:5?A8�HA7XA6��A6M�A5?}A5
=A45?A3��A3hsA3\)A1�A0��A0I�A.��A.�A-oA,jA,1'A+��A*��A)33A(��A'��A'
=A&��A&Q�A%�^A$�A#|�A"n�A!�A!`BA ��A z�A�wAr�A��AQ�A�AQ�AoA��AS�AM�A��A��A;dA�A��A�DA�+AQ�A��AdZAoA��AM�A�mA�AC�A+A�A��A=qA��A��A�A`BA�AjAJAl�A
{A�\A�AA�AȴA��AjA�A��AjA ~�@�M�@���@���@��^@�1'@��@��@���@���@�!@��@�n�@���@�9X@���@�^5@��@���@�9X@�"�@ޟ�@�J@ݲ-@�/@��;@ڰ!@�M�@��@�Q�@�ff@ՙ�@�V@�Z@�K�@Ұ!@�ff@���@�X@���@�  @���@̃@�\)@ʸR@ʇ+@�M�@�{@��T@�O�@ǥ�@���@��@�@ŉ7@őh@�/@���@�
=@��@���@�j@�  @�|�@�~�@�n�@�^5@�^5@�5?@�-@��-@��u@�"�@��7@���@���@��!@���@�ff@��@��@�Q�@��
@�t�@�o@��@�t�@��j@�b@�@���@�(�@��w@�l�@�C�@��R@�n�@�@��h@��@��`@�r�@��;@�"�@�M�@�p�@��@��`@��@��@�|�@�33@���@��@�?}@���@�Ĝ@��@�z�@�(�@��m@�ƨ@���@��P@�|�@�t�@�dZ@�C�@�"�@��@���@���@��@��@��@��R@��R@��\@�~�@�n�@�^5@��-@�X@��@���@�o@�5?@��#@���@�G�@��j@�r�@�A�@�b@��F@�\)@��@���@�n�@��@��#@��#@���@��@�7L@�%@��@�(�@�1@���@��m@��;@��;@��
@���@�ƨ@��F@��P@��P@�\)@�^5@��T@���@��h@��7@�x�@�x�@�O�@���@�  @�33@�^5@�@���@���@���@���@���@���@��7@���@�z�@�Z@�1@��@���@��@���@��H@�ȴ@���@��+@�ff@�V@�n�@�~�@�n�@�J@���@�@��^@��h@�p�@�X@�/@��/@��j@��u@��D@�Z@��@~�@~E�@}p�@|��@|j@|I�@|�@|1@{�m@{��@{"�@zJ@yx�@x�9@xr�@xA�@x1'@w�w@vȴ@v�+@vE�@v{@u�T@u�@t�/@s�F@r�H@r~�@q��@p��@p�9@pA�@o�@o�@o�;@o��@o��@o|�@o|�@ol�@o\)@o\)@oK�@o;d@o;d@o;d@o;d@n�y@n�R@nȴ@n�+@m��@k�
@k��@kt�@kt�@kt�@kt�@kt�@j�H@jJ@i�^@i�7@i7L@h��@hĜ@h�@hQ�@h1'@g;d@f5?@f{@e�@e�T@e@e�-@e�h@e�@d�@d(�@c�m@c��@cdZ@cC�@c@b��@a�@a�7@aG�@a%@`�9@`Q�@_��@_�P@^�y@^{@]�@]?}@]V@\�@\(�@[�m@Z�H@Z=q@Y�^@X��@XbN@X1'@X  @W;d@U��@T��@T��@T�/@T�/@S��@S33@S33@So@R�\@Q�@Q�^@R=q@S"�@S33@SS�@R�H@R��@S@S@R~�@Q�@Q�#@Q��@Q�@P1'@N�y@Nv�@NE�@M�T@M�-@M�@L��@L�/@L�D@L9X@L1@K��@KC�@K@J�@J��@J�\@J~�@J^5@J-@JJ@I��@Ix�@IG�@I&�@H��@HQ�@G�@G�@G�;@HĜ@HĜ@Hr�@H  @H  @H  @G�;@G�w@G�@G�w@G�P@G\)@G�@F��@F�y@F�y@F�y@F�@F�R@E�T@E`B@E`B@E`B@E`B@EV@D9X@Cƨ@Co@BM�@A�#@A��@A��@Ax�@A�@@�`@@bN@@A�@@ �@@  @?��@?l�@?\)@?K�@?;d@>�+@>@=�-@=��@=�@=p�@=/@<��@<�D@<1@;�F@;�F@;��@;dZ@:�@:�\@:^5@:J@9�^@9G�@9%@8��@8r�@8  @7�;@7�w@7��@7|�@7\)@7;d@6�y@6�@6�R@6��@6��@6ff@6$�@6@5?}@4��@4I�@3��@3��@3��@3�@3@2�\@1��@1��@1�^@1��@1hs@1&�@1%@1%@0��@0��@0�9@0bN@0  @/�@/��@/�w@/�w@/�w@/�w@/�w@/�w@/��@/\)@/
=@.��@.E�@.{@.{@-�T@-�h@-p�@-O�@-V@,��@,�/@,��@,�@+��@+t�@+S�@+"�@+@*�H@*�\@)��@)��@)%@(bN@(b@'�w@'��@'K�@'�@&��@&�R@&v�@&@%�-@%�@%/@$�j@$�D@$j@$I�@$9X@#�F@#33@#@"n�@!�^@!��@!&�@ �`@ �`@ ��@ ��@ 1'@��@�w@�@��@�P@|�@l�@K�@K�@+@
=@�@�R@��@��@��@��@��@�R@��@�+@ff@V@5?@��@�-@��@`B@��@z�@9X@�@��@�m@�m@ƨ@C�@�@�!@�!@�\@�\@�\@~�@n�@-@��@hs@X@7L@�@r�@�;@�P@;d@�y@�@�R@v�@5?@@�@?}@/@/@V@�/@�D@Z@(�@��@��@dZ@dZ@S�@C�@C�@C�@33@�@�H@�H@��@��@^5@=q@��@�#@�^@��@X@�`@�u@r�@A�@�;@K�@��@ȴ@v�@V@5?@{@�@��@�-@p�@`B@/@V@V@V@�@�/@��@��@��@��@��@��@��@��@9X@�m@
�@
n�@
M�@
=q@
-@
�@
J@	&�@Ĝ@�9@�9@��@��@r�@Q�@A�@1'@ �@  @b@b@  @�@�@��@�@�P@l�@;d@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��\A�/A��A���A���A��A��A�~�A�r�A�+A��A��jA�l�A�M�A�/A�  A���A���A���A��uA��hA��A�~�A�t�A�n�A�dZA�K�A�7LA�&�A��A���A��A��HA���A�C�A�O�A��A�`BA��A�O�A�(�A���A���A��A�7LA��A��A�=qA���A�=qA��`A��^A��A�VA�-A�\)A��
A�~�A�I�A�ȴA�I�A��hA�9XA��A�ZA���A���A��A���A�bNA�JA�M�A��jA�z�A�JA���A�v�A��TA��A��jA�x�A�E�A�ĜA�+A��A�;dA��A�M�A��wA��uA�t�A�jA�(�A��\A�K�A�&�A���A��\A��A�hsA�A�|�A��mA��A�;dA�n�A���A�ZA��-A���A���A��A�(�A��A�O�A���A���A�  A���A��A��A�oA��mA���A�I�A��yA�^5A�9XA��A�ƨA�(�A��TA��PA~�`A}|�A|-Ay�AvȴAu�As��Aq�Ao�-Am�FAk��Aj��Ai�FAhbAf�DAdQ�Aa�A[�
AW��AWoAV9XAU/AT��AS��AS�AS?}AQ�mAQ33AO��AM�
AK�AK+AJ��AI�mAHȴAF��AF�AE��AD��ADr�AC/ABr�AA�^A?�A=G�A;hsA:��A:5?A8�HA7XA6��A6M�A5?}A5
=A45?A3��A3hsA3\)A1�A0��A0I�A.��A.�A-oA,jA,1'A+��A*��A)33A(��A'��A'
=A&��A&Q�A%�^A$�A#|�A"n�A!�A!`BA ��A z�A�wAr�A��AQ�A�AQ�AoA��AS�AM�A��A��A;dA�A��A�DA�+AQ�A��AdZAoA��AM�A�mA�AC�A+A�A��A=qA��A��A�A`BA�AjAJAl�A
{A�\A�AA�AȴA��AjA�A��AjA ~�@�M�@���@���@��^@�1'@��@��@���@���@�!@��@�n�@���@�9X@���@�^5@��@���@�9X@�"�@ޟ�@�J@ݲ-@�/@��;@ڰ!@�M�@��@�Q�@�ff@ՙ�@�V@�Z@�K�@Ұ!@�ff@���@�X@���@�  @���@̃@�\)@ʸR@ʇ+@�M�@�{@��T@�O�@ǥ�@���@��@�@ŉ7@őh@�/@���@�
=@��@���@�j@�  @�|�@�~�@�n�@�^5@�^5@�5?@�-@��-@��u@�"�@��7@���@���@��!@���@�ff@��@��@�Q�@��
@�t�@�o@��@�t�@��j@�b@�@���@�(�@��w@�l�@�C�@��R@�n�@�@��h@��@��`@�r�@��;@�"�@�M�@�p�@��@��`@��@��@�|�@�33@���@��@�?}@���@�Ĝ@��@�z�@�(�@��m@�ƨ@���@��P@�|�@�t�@�dZ@�C�@�"�@��@���@���@��@��@��@��R@��R@��\@�~�@�n�@�^5@��-@�X@��@���@�o@�5?@��#@���@�G�@��j@�r�@�A�@�b@��F@�\)@��@���@�n�@��@��#@��#@���@��@�7L@�%@��@�(�@�1@���@��m@��;@��;@��
@���@�ƨ@��F@��P@��P@�\)@�^5@��T@���@��h@��7@�x�@�x�@�O�@���@�  @�33@�^5@�@���@���@���@���@���@���@��7@���@�z�@�Z@�1@��@���@��@���@��H@�ȴ@���@��+@�ff@�V@�n�@�~�@�n�@�J@���@�@��^@��h@�p�@�X@�/@��/@��j@��u@��D@�Z@��@~�@~E�@}p�@|��@|j@|I�@|�@|1@{�m@{��@{"�@zJ@yx�@x�9@xr�@xA�@x1'@w�w@vȴ@v�+@vE�@v{@u�T@u�@t�/@s�F@r�H@r~�@q��@p��@p�9@pA�@o�@o�@o�;@o��@o��@o|�@o|�@ol�@o\)@o\)@oK�@o;d@o;d@o;d@o;d@n�y@n�R@nȴ@n�+@m��@k�
@k��@kt�@kt�@kt�@kt�@kt�@j�H@jJ@i�^@i�7@i7L@h��@hĜ@h�@hQ�@h1'@g;d@f5?@f{@e�@e�T@e@e�-@e�h@e�@d�@d(�@c�m@c��@cdZ@cC�@c@b��@a�@a�7@aG�@a%@`�9@`Q�@_��@_�P@^�y@^{@]�@]?}@]V@\�@\(�@[�m@Z�H@Z=q@Y�^@X��@XbN@X1'@X  @W;d@U��@T��@T��@T�/@T�/@S��@S33@S33@So@R�\@Q�@Q�^@R=q@S"�@S33@SS�@R�H@R��@S@S@R~�@Q�@Q�#@Q��@Q�@P1'@N�y@Nv�@NE�@M�T@M�-@M�@L��@L�/@L�D@L9X@L1@K��@KC�@K@J�@J��@J�\@J~�@J^5@J-@JJ@I��@Ix�@IG�@I&�@H��@HQ�@G�@G�@G�;@HĜ@HĜ@Hr�@H  @H  @H  @G�;@G�w@G�@G�w@G�P@G\)@G�@F��@F�y@F�y@F�y@F�@F�R@E�T@E`B@E`B@E`B@E`B@EV@D9X@Cƨ@Co@BM�@A�#@A��@A��@Ax�@A�@@�`@@bN@@A�@@ �@@  @?��@?l�@?\)@?K�@?;d@>�+@>@=�-@=��@=�@=p�@=/@<��@<�D@<1@;�F@;�F@;��@;dZ@:�@:�\@:^5@:J@9�^@9G�@9%@8��@8r�@8  @7�;@7�w@7��@7|�@7\)@7;d@6�y@6�@6�R@6��@6��@6ff@6$�@6@5?}@4��@4I�@3��@3��@3��@3�@3@2�\@1��@1��@1�^@1��@1hs@1&�@1%@1%@0��@0��@0�9@0bN@0  @/�@/��@/�w@/�w@/�w@/�w@/�w@/�w@/��@/\)@/
=@.��@.E�@.{@.{@-�T@-�h@-p�@-O�@-V@,��@,�/@,��@,�@+��@+t�@+S�@+"�@+@*�H@*�\@)��@)��@)%@(bN@(b@'�w@'��@'K�@'�@&��@&�R@&v�@&@%�-@%�@%/@$�j@$�D@$j@$I�@$9X@#�F@#33@#@"n�@!�^@!��@!&�@ �`@ �`@ ��@ ��@ 1'@��@�w@�@��@�P@|�@l�@K�@K�@+@
=@�@�R@��@��@��@��@��@�R@��@�+@ff@V@5?@��@�-@��@`B@��@z�@9X@�@��@�m@�m@ƨ@C�@�@�!@�!@�\@�\@�\@~�@n�@-@��@hs@X@7L@�@r�@�;@�P@;d@�y@�@�R@v�@5?@@�@?}@/@/@V@�/@�D@Z@(�@��@��@dZ@dZ@S�@C�@C�@C�@33@�@�H@�H@��@��@^5@=q@��@�#@�^@��@X@�`@�u@r�@A�@�;@K�@��@ȴ@v�@V@5?@{@�@��@�-@p�@`B@/@V@V@V@�@�/@��@��@��@��@��@��@��@��@9X@�m@
�@
n�@
M�@
=q@
-@
�@
J@	&�@Ĝ@�9@�9@��@��@r�@Q�@A�@1'@ �@  @b@b@  @�@�@��@�@�P@l�@;d@�y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB+BoB/B,B+B)�B)�B)�B)�B(�B'�B'�B%�B$�B%�B&�B&�B%�B%�B%�B%�B%�B'�B+B+B.B/B-B+B)�B'�B&�B%�B#�B"�B�B�B�B�B�B�B�B�B�B�BhBhB\BDB	7B+B%B%BBB��B��B��B�B�B�B�yB�mB�fB�`B�sB�sB�B�B�B�sB�)B��B��B��BŢB�?B�'B�RB�RB�RB�qB�XB��B��B�1BjB]/BO�BL�BJ�BI�BI�B@�B9XB5?B1'B+B�B�BPBB�B�B��B�}B�-B�B��B�hB�1Bq�BYBE�B;dB0!B"�B�B%B
��B
�B
�B
ƨB
��B
��B
�uB
�=B
�+B
�B
w�B
bNB
]/B
YB
J�B
A�B
8RB
(�B
�B
DB
B	��B	�B	�BB	�B	��B	��B	��B	�FB	��B	�uB	� B	bNB	_;B	\)B	W
B	R�B	O�B	L�B	K�B	E�B	?}B	9XB	0!B	&�B	 �B	�B	�B	�B	PB	1B	%B	B	B��B��B��B�B�ZB�)B�B�
B�B��B��B��BǮBŢBÖB��B��B��B�wB�RB�?B�B�B��B��B��B��B��B��B�uB�\B�JB�DB�=B�1B�B� B}�B{�By�Bw�Bu�Bs�Bp�Bk�BiyBffBcTBbNB^5B]/BZBYBVBR�BP�BP�BO�BO�BN�BL�BJ�BI�BH�BH�BI�BI�BH�BH�BH�BH�BH�BG�BG�BF�BF�BE�BD�BB�B@�B;dB6FB49B33B2-B1'B/B/B,B)�B)�B,B+B)�B)�B'�B&�B&�B&�B(�B(�B'�B.B1'B5?B6FB6FB5?B2-B9XB9XB9XB:^B;dB;dB;dB;dB=qB>wB=qBA�BD�BD�BG�BK�BN�BP�BQ�BS�BVBVBVBZB\)B\)B\)B[#BZBYBXBVBS�BQ�BS�BT�BW
BW
BXB\)B[#B_;B_;B_;B`BB_;B^5BaHBiyBk�Bl�Bl�Bp�Br�Bs�Bs�Bx�Bx�Bx�Bx�Bx�Bz�B~�B~�B� B�B� B~�B�B�=B�1B�JB�PB�\B�oB�uB�uB��B��B��B��B��B��B��B��B��B��B�B�B�FB�RB�XB�^B�dB�}BĜBȴB��B��B��B��B��B�B�
B�B�B�#B�#B�#B�)B�/B�5B�BB�fB�B��B��B��B��B��B��B��B��B	  B	B	B	%B	+B	PB	\B	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	$�B	'�B	(�B	+B	.B	0!B	1'B	1'B	2-B	2-B	2-B	2-B	2-B	33B	33B	49B	49B	49B	9XB	:^B	;dB	;dB	;dB	<jB	<jB	=qB	?}B	@�B	A�B	B�B	E�B	F�B	F�B	G�B	G�B	G�B	G�B	H�B	L�B	M�B	M�B	Q�B	T�B	YB	^5B	`BB	`BB	aHB	cTB	cTB	e`B	gmB	hsB	hsB	jB	o�B	q�B	q�B	q�B	s�B	v�B	w�B	y�B	}�B	~�B	� B	� B	�B	�+B	�DB	�PB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	��B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B

=B

=B
DB
JB
JB
JB
PB
PB
VB
\B
bB
\B
\B
VB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
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
&�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
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
/B
0!B
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
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
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
M�B
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
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
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
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
gmB
gmB
gmB
gmB
hsB
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
iyB
jB
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB+BoB/B,B+B)�B)�B)�B)�B(�B'�B'�B%�B$�B%�B&�B&�B%�B%�B%�B%�B%�B'�B+B+B.B/B-B+B)�B'�B&�B%�B#�B"�B�B�B�B�B�B�B�B�B�B�BhBhB\BDB	7B+B%B%BBB��B��B��B�B�B�B�yB�mB�fB�`B�sB�sB�B�B�B�sB�)B��B��B��BŢB�?B�'B�RB�RB�RB�qB�XB��B��B�1BjB]/BO�BL�BJ�BI�BI�B@�B9XB5?B1'B+B�B�BPBB�B�B��B�}B�-B�B��B�hB�1Bq�BYBE�B;dB0!B"�B�B%B
��B
�B
�B
ƨB
��B
��B
�uB
�=B
�+B
�B
w�B
bNB
]/B
YB
J�B
A�B
8RB
(�B
�B
DB
B	��B	�B	�BB	�B	��B	��B	��B	�FB	��B	�uB	� B	bNB	_;B	\)B	W
B	R�B	O�B	L�B	K�B	E�B	?}B	9XB	0!B	&�B	 �B	�B	�B	�B	PB	1B	%B	B	B��B��B��B�B�ZB�)B�B�
B�B��B��B��BǮBŢBÖB��B��B��B�wB�RB�?B�B�B��B��B��B��B��B��B�uB�\B�JB�DB�=B�1B�B� B}�B{�By�Bw�Bu�Bs�Bp�Bk�BiyBffBcTBbNB^5B]/BZBYBVBR�BP�BP�BO�BO�BN�BL�BJ�BI�BH�BH�BI�BI�BH�BH�BH�BH�BH�BG�BG�BF�BF�BE�BD�BB�B@�B;dB6FB49B33B2-B1'B/B/B,B)�B)�B,B+B)�B)�B'�B&�B&�B&�B(�B(�B'�B.B1'B5?B6FB6FB5?B2-B9XB9XB9XB:^B;dB;dB;dB;dB=qB>wB=qBA�BD�BD�BG�BK�BN�BP�BQ�BS�BVBVBVBZB\)B\)B\)B[#BZBYBXBVBS�BQ�BS�BT�BW
BW
BXB\)B[#B_;B_;B_;B`BB_;B^5BaHBiyBk�Bl�Bl�Bp�Br�Bs�Bs�Bx�Bx�Bx�Bx�Bx�Bz�B~�B~�B� B�B� B~�B�B�=B�1B�JB�PB�\B�oB�uB�uB��B��B��B��B��B��B��B��B��B��B�B�B�FB�RB�XB�^B�dB�}BĜBȴB��B��B��B��B��B�B�
B�B�B�#B�#B�#B�)B�/B�5B�BB�fB�B��B��B��B��B��B��B��B��B	  B	B	B	%B	+B	PB	\B	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	$�B	'�B	(�B	+B	.B	0!B	1'B	1'B	2-B	2-B	2-B	2-B	2-B	33B	33B	49B	49B	49B	9XB	:^B	;dB	;dB	;dB	<jB	<jB	=qB	?}B	@�B	A�B	B�B	E�B	F�B	F�B	G�B	G�B	G�B	G�B	H�B	L�B	M�B	M�B	Q�B	T�B	YB	^5B	`BB	`BB	aHB	cTB	cTB	e`B	gmB	hsB	hsB	jB	o�B	q�B	q�B	q�B	s�B	v�B	w�B	y�B	}�B	~�B	� B	� B	�B	�+B	�DB	�PB	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	��B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B

=B

=B
DB
JB
JB
JB
PB
PB
VB
\B
bB
\B
\B
VB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
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
&�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
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
/B
0!B
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
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
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
M�B
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
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
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
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
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
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
gmB
gmB
gmB
gmB
hsB
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
iyB
jB
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       * N000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230524120104                                          AO  ARCAADJP                                                                    20230524120104    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230524120104  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230524120104  QCF$                G�O�G�O�G�O�0               