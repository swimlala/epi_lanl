CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-08T22:31:10Z creation      
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
_FillValue                 �  _$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̄   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230608223110  20230608223110  5904702 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  5449                            2B  A   NAVIS_A                         0470                            011514                          863 @ا"�B"1   @ا}'ܐ@7�-�d�����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F       @�33@�  @���A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C�C
  C  C��C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�C3Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@�G�A
>A$��AC
=Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�BB(�B!(�B(B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C c�Cc�CJ=CJ=Cc�C
J=CJ=C
CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,0�C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�D�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��D��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�D�FD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�D�IHDHD��HD�	HD�IHDÉHD��HD�	HD�IHDĉHD��HD�	HD�IHDŉHD��HD�	HD�IHDƉHD��HD�	HD�IHDǉHD��HD�	HD�IHDȉHD��HD�	HD�IHDɉHD��HD�	HD�L{DʉHD��HD�	HD�IHDˉHD��HD�	HD�IHD̉HD��HD�	HD�IHD͉HD��HD�{D�IHDΉHD��HD�	HD�IHDωHD��HD�	HD�IHDЉHD��HD�	HD�IHDщHD��HD�	HD�IHD҉HD��HD�	HD�IHDӉHD��HD�	HD�IHDԉHD��HD�	HD�IHDՉHD��HD�	HD�IHD։HD��HD�	HD�IHD׉HD��HD�	HD�IHD؉HD��HD�	HD�IHDىHD��HD�	HD�IHDډHD��HD�	HD�IHDۉHD��HD�	HD�IHD܉HD��HD�	HD�IHD݉HD��HD�	HD�IHDމHD��HD�	HD�IHD߉HD��HD�	HD�IHD��HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD��HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD�HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD�	HD�IHD��HD��HD��D�/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A���A���A���A���A�  A�JA�A�1A�A�A�%A�%A�1A�1A�
=A�
=A�JA�bA�VA��A��^A��\A��DA��+A��A��A��+A��A��A��A�z�A�l�A�dZA��A��^A��wA��^A��^A��9A��RA��RA��^A��^A��RA��RA��FA��RA��9A��A���A�l�A��A�{A��A��^A�G�A�$�A��A���A���A��A�v�A�VA�x�A��jA��TA���A�7LA�A�A��PA��RA�?}A�JA��A��RA��-A��A���A�bNA�E�A�=qA�5?A�{A���A�l�A��mA�  A�r�A�Q�A��#A�t�A�1A�O�A�C�A�z�A���A��PA�?}A�%A�oA���A�ZA�x�A��/A�`BA��A�ȴA��uA��A�bNA�M�A���A�%A��^A�A�A���A��A���A���A�9XA��A�$�A�(�A�  A��uA�(�A�ffA���A��A�FAK�AA~=qA|�A{�FAzbNAy��Ay33Av�Au%As�-Aq�Ao33Al��Akt�Aj$�Ah��Ag\)AeC�Ad �Ab�HAbE�Aa�FA]��A[�A[��AZ�`AY�AV�uAR�/AQ�AQp�AP�RAO��AO�AN�uAN  AM�wAMO�AL��AJ�!AI�AGƨAEK�ACC�ABbNAA�wAAXAA&�AA
=A@ȴA@{A?��A?O�A>��A>  A=hsA=XA=VA<r�A;��A;VA:A81'A5��A5�FA4ĜA3�
A3+A2��A2�yA2�`A2ȴA2��A25?A1&�A.��A-�A,��A+K�A)A(VA'��A'l�A&�+A& �A%��A%��A$jA#"�A!�mA ��A VA��Az�AO�A �A�A�FAVAȴA�AC�A��A^5A{A�FA�A;dA
=A�\A(�A�FA+AjA
=AZAS�A��A��A�uAl�A�A
$�A	��A	C�A	�A��AjAA�A��A�AoA9XA�AoA�#Ax�A ��A A�@��@��@��7@���@�\@�h@��D@�@�?}@��/@�bN@�b@���@띲@땁@�@�t�@�;d@�
=@�\@陚@��@�I�@�-@�+@�X@�7L@��@���@���@�Ĝ@ܴ9@ܬ@ܛ�@�9X@�S�@ڟ�@�@�&�@��m@�l�@ָR@���@�%@�r�@�  @ӥ�@�+@�ff@�K�@�I�@��@�p�@�O�@��@Ǯ@���@�?}@��/@�
=@��@�{@���@��/@�z�@�(�@��;@��P@�33@��@���@�E�@�X@���@�ƨ@��H@�ff@�-@���@�hs@���@�b@��F@���@�|�@�t�@�dZ@�K�@�o@�n�@���@��@�|�@��H@�O�@��@��m@�dZ@�{@�%@���@�r�@�A�@�1@���@��-@�&�@��/@��@�z�@�Z@��;@�C�@�@���@�~�@��@���@�b@���@�S�@�
=@���@�G�@�K�@�~�@�J@�?}@��`@�I�@�l�@��@��@�@��-@��h@��@�X@�&�@���@��u@�A�@��@�"�@���@��@���@���@���@�Z@�K�@�o@��H@���@��+@�ff@�J@��^@�x�@�&�@�V@���@��j@�C�@���@�-@�x�@��`@�9X@��;@�ƨ@��F@���@�|�@�\)@�\)@�K�@�;d@�o@�"�@�+@�l�@�|�@�;d@��\@�5?@�{@��@���@��@���@���@��j@��9@���@�bN@�Q�@�1@��w@���@�dZ@�+@��@��@���@��!@��\@�~�@�ff@�^5@�V@�V@�=q@��T@��^@��@�1@|j@v��@u�-@up�@u/@t��@t�j@t�D@tz�@tZ@tZ@tZ@t9X@s��@st�@so@rn�@q��@q��@q��@qG�@q&�@q%@p��@p�9@p�u@pr�@pbN@pQ�@pA�@p1'@pb@o�@o
=@n�R@nff@nV@nE�@m��@m?}@lZ@k33@jM�@i�@h1'@g�@g��@g�@g�P@g�P@gl�@g+@e�-@d�j@d��@d1@c��@c��@c��@c�m@c�
@c��@b�!@b-@bJ@a�#@a�7@`��@`��@`A�@` �@_��@_;d@^ȴ@^�+@^V@^5?@^$�@^{@^{@^@]�@]�T@]�-@]O�@]�@\Z@[�
@[C�@["�@[o@[@[@Z�!@Zn�@ZM�@Y�@Y7L@X�9@X�9@X�9@X�9@X�9@X�9@X�9@X�u@Xr�@XQ�@W�@Wl�@V�y@VV@U�T@U�-@U�h@U`B@T�@TI�@T�@S��@S�@St�@SS�@S33@R�H@R��@R��@R�!@R��@R�\@R�\@R~�@R^5@Q��@Q��@Q��@Qx�@Qhs@QG�@P��@PA�@O�;@O|�@O
=@N�@N�@N�@N�y@N�@NE�@Mp�@MV@L�/@L�j@L��@Lz�@L9X@L(�@K�F@Kt�@KdZ@K"�@K@J��@J�\@Jn�@I�#@Ix�@HbN@H  @G�@F��@F$�@E@E�-@E��@E�@E`B@EV@D�/@D�j@D�j@D�j@D�@Dj@C��@C�F@C�F@C��@C�@C@B��@B�@A��@AX@A%@A�@A%@@��@@�9@@Q�@@b@?�w@>�@>5?@<�@<z�@;��@;ƨ@;�F@;ƨ@;��@;�@:�H@:��@:��@:-@:J@9�7@9hs@9%@8�u@8Q�@8A�@8  @7�;@7�w@7�w@7�@7|�@7l�@7\)@7+@7
=@6�y@6�R@6{@5�-@5O�@4��@4I�@3�m@3��@3�@3S�@2�@2��@2��@2=q@17L@0��@0Q�@0A�@0 �@0b@0  @/�@/�@/��@/�@/l�@/
=@.�R@.V@.5?@.$�@-��@-V@,�@,��@+�m@+C�@+o@*�@*��@)��@)�#@)�^@)�7@)X@)7L@)&�@)%@(r�@( �@'|�@';d@'�@'
=@&�y@&�@&�R@&��@&�+@&{@%��@%O�@%?}@%?}@%?}@%/@%/@%�@$��@$z�@$z�@$z�@$j@$I�@#��@#��@#C�@"��@"�\@!�^@ ��@ �`@ �`@ �9@ Q�@��@|�@�@E�@`B@�/@I�@�@ƨ@t�@33@�@��@��@��@�^@��@�7@G�@G�@G�@G�@�9@�u@�@�u@b@��@�w@�@�@�@�@�@��@��@�P@|�@|�@l�@\)@K�@K�@+@��@�y@�@�R@��@V@V@E�@V@V@E�@E�@V@E�@{@�T@�h@�@�j@j@9X@1@ƨ@��@S�@C�@"�@��@n�@��@�@�#@��@X@G�@�@��@Ĝ@bN@A�@A�@b@��@�w@�@\)@+@�@��@�@ȴ@ȴ@��@V@$�@{@@@O�@��@��@j@9X@�@�@�m@�m@�F@��@��@�@S�@33@@
��@
=q@	��@	�7@	7L@	�@��@��@�`@�`@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�@Q�@Q�@A�@1'@ �@ �@ �@b@  @��@�@�@��@\)@�+@{@��@�h@?}@/@V@��@��@��@�/@��@�j@��@j@I�@(�@�@�m@ƨ@�F@��@��@��@��@��@��@�@�@t�@t�@t�@t�@dZ@33@�\@^5@M�@M�@M�@M�@=q@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A���A���A���A���A�  A�JA�A�1A�A�A�%A�%A�1A�1A�
=A�
=A�JA�bA�VA��A��^A��\A��DA��+A��A��A��+A��A��A��A�z�A�l�A�dZA��A��^A��wA��^A��^A��9A��RA��RA��^A��^A��RA��RA��FA��RA��9A��A���A�l�A��A�{A��A��^A�G�A�$�A��A���A���A��A�v�A�VA�x�A��jA��TA���A�7LA�A�A��PA��RA�?}A�JA��A��RA��-A��A���A�bNA�E�A�=qA�5?A�{A���A�l�A��mA�  A�r�A�Q�A��#A�t�A�1A�O�A�C�A�z�A���A��PA�?}A�%A�oA���A�ZA�x�A��/A�`BA��A�ȴA��uA��A�bNA�M�A���A�%A��^A�A�A���A��A���A���A�9XA��A�$�A�(�A�  A��uA�(�A�ffA���A��A�FAK�AA~=qA|�A{�FAzbNAy��Ay33Av�Au%As�-Aq�Ao33Al��Akt�Aj$�Ah��Ag\)AeC�Ad �Ab�HAbE�Aa�FA]��A[�A[��AZ�`AY�AV�uAR�/AQ�AQp�AP�RAO��AO�AN�uAN  AM�wAMO�AL��AJ�!AI�AGƨAEK�ACC�ABbNAA�wAAXAA&�AA
=A@ȴA@{A?��A?O�A>��A>  A=hsA=XA=VA<r�A;��A;VA:A81'A5��A5�FA4ĜA3�
A3+A2��A2�yA2�`A2ȴA2��A25?A1&�A.��A-�A,��A+K�A)A(VA'��A'l�A&�+A& �A%��A%��A$jA#"�A!�mA ��A VA��Az�AO�A �A�A�FAVAȴA�AC�A��A^5A{A�FA�A;dA
=A�\A(�A�FA+AjA
=AZAS�A��A��A�uAl�A�A
$�A	��A	C�A	�A��AjAA�A��A�AoA9XA�AoA�#Ax�A ��A A�@��@��@��7@���@�\@�h@��D@�@�?}@��/@�bN@�b@���@띲@땁@�@�t�@�;d@�
=@�\@陚@��@�I�@�-@�+@�X@�7L@��@���@���@�Ĝ@ܴ9@ܬ@ܛ�@�9X@�S�@ڟ�@�@�&�@��m@�l�@ָR@���@�%@�r�@�  @ӥ�@�+@�ff@�K�@�I�@��@�p�@�O�@��@Ǯ@���@�?}@��/@�
=@��@�{@���@��/@�z�@�(�@��;@��P@�33@��@���@�E�@�X@���@�ƨ@��H@�ff@�-@���@�hs@���@�b@��F@���@�|�@�t�@�dZ@�K�@�o@�n�@���@��@�|�@��H@�O�@��@��m@�dZ@�{@�%@���@�r�@�A�@�1@���@��-@�&�@��/@��@�z�@�Z@��;@�C�@�@���@�~�@��@���@�b@���@�S�@�
=@���@�G�@�K�@�~�@�J@�?}@��`@�I�@�l�@��@��@�@��-@��h@��@�X@�&�@���@��u@�A�@��@�"�@���@��@���@���@���@�Z@�K�@�o@��H@���@��+@�ff@�J@��^@�x�@�&�@�V@���@��j@�C�@���@�-@�x�@��`@�9X@��;@�ƨ@��F@���@�|�@�\)@�\)@�K�@�;d@�o@�"�@�+@�l�@�|�@�;d@��\@�5?@�{@��@���@��@���@���@��j@��9@���@�bN@�Q�@�1@��w@���@�dZ@�+@��@��@���@��!@��\@�~�@�ff@�^5@�V@�V@�=q@��T@��^@��@�1@|j@v��@u�-@up�@u/@t��@t�j@t�D@tz�@tZ@tZ@tZ@t9X@s��@st�@so@rn�@q��@q��@q��@qG�@q&�@q%@p��@p�9@p�u@pr�@pbN@pQ�@pA�@p1'@pb@o�@o
=@n�R@nff@nV@nE�@m��@m?}@lZ@k33@jM�@i�@h1'@g�@g��@g�@g�P@g�P@gl�@g+@e�-@d�j@d��@d1@c��@c��@c��@c�m@c�
@c��@b�!@b-@bJ@a�#@a�7@`��@`��@`A�@` �@_��@_;d@^ȴ@^�+@^V@^5?@^$�@^{@^{@^@]�@]�T@]�-@]O�@]�@\Z@[�
@[C�@["�@[o@[@[@Z�!@Zn�@ZM�@Y�@Y7L@X�9@X�9@X�9@X�9@X�9@X�9@X�9@X�u@Xr�@XQ�@W�@Wl�@V�y@VV@U�T@U�-@U�h@U`B@T�@TI�@T�@S��@S�@St�@SS�@S33@R�H@R��@R��@R�!@R��@R�\@R�\@R~�@R^5@Q��@Q��@Q��@Qx�@Qhs@QG�@P��@PA�@O�;@O|�@O
=@N�@N�@N�@N�y@N�@NE�@Mp�@MV@L�/@L�j@L��@Lz�@L9X@L(�@K�F@Kt�@KdZ@K"�@K@J��@J�\@Jn�@I�#@Ix�@HbN@H  @G�@F��@F$�@E@E�-@E��@E�@E`B@EV@D�/@D�j@D�j@D�j@D�@Dj@C��@C�F@C�F@C��@C�@C@B��@B�@A��@AX@A%@A�@A%@@��@@�9@@Q�@@b@?�w@>�@>5?@<�@<z�@;��@;ƨ@;�F@;ƨ@;��@;�@:�H@:��@:��@:-@:J@9�7@9hs@9%@8�u@8Q�@8A�@8  @7�;@7�w@7�w@7�@7|�@7l�@7\)@7+@7
=@6�y@6�R@6{@5�-@5O�@4��@4I�@3�m@3��@3�@3S�@2�@2��@2��@2=q@17L@0��@0Q�@0A�@0 �@0b@0  @/�@/�@/��@/�@/l�@/
=@.�R@.V@.5?@.$�@-��@-V@,�@,��@+�m@+C�@+o@*�@*��@)��@)�#@)�^@)�7@)X@)7L@)&�@)%@(r�@( �@'|�@';d@'�@'
=@&�y@&�@&�R@&��@&�+@&{@%��@%O�@%?}@%?}@%?}@%/@%/@%�@$��@$z�@$z�@$z�@$j@$I�@#��@#��@#C�@"��@"�\@!�^@ ��@ �`@ �`@ �9@ Q�@��@|�@�@E�@`B@�/@I�@�@ƨ@t�@33@�@��@��@��@�^@��@�7@G�@G�@G�@G�@�9@�u@�@�u@b@��@�w@�@�@�@�@�@��@��@�P@|�@|�@l�@\)@K�@K�@+@��@�y@�@�R@��@V@V@E�@V@V@E�@E�@V@E�@{@�T@�h@�@�j@j@9X@1@ƨ@��@S�@C�@"�@��@n�@��@�@�#@��@X@G�@�@��@Ĝ@bN@A�@A�@b@��@�w@�@\)@+@�@��@�@ȴ@ȴ@��@V@$�@{@@@O�@��@��@j@9X@�@�@�m@�m@�F@��@��@�@S�@33@@
��@
=q@	��@	�7@	7L@	�@��@��@�`@�`@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@�@Q�@Q�@A�@1'@ �@ �@ �@b@  @��@�@�@��@\)@�+@{@��@�h@?}@/@V@��@��@��@�/@��@�j@��@j@I�@(�@�@�m@ƨ@�F@��@��@��@��@��@��@�@�@t�@t�@t�@t�@dZ@33@�\@^5@M�@M�@M�@M�@=q@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B%B%B+B1B+B+BB%B+B+B+B+B+B1B1B+B+B+B+B+B1BDBPBVB\B\BbBbBbBhBhBhBhBhBhBhBhBhBhBhBbBbBbBbBbBbBbBbBbBhBhBoB�B�B�B�B �B"�B"�B%�B-B-B;dB?}B<jB:^B7LB0!B.B/B.B)�B'�B&�B%�B%�B%�B$�B#�B"�B!�B �B�B�B�B�BoBDBB�B�sB�BŢB�'B��B��B��B�\B�DB�+By�B[#BI�BB�B>wB;dB9XB7LB5?B49B2-B0!B)�B�BBB  B��B�B�ZB�/B��B�9B��B~�Bk�BZBN�BF�B=qB.B+B'�B!�B�BhB1BB
��B
�B
�HB
�
B
��B
�dB
�B
��B
��B
�oB
�1B
z�B
r�B
k�B
ffB
_;B
G�B
;dB
8RB
1'B
&�B
oB
B
B
B
B	��B	��B	��B	��B	�B	�B	�B	�5B	�
B	��B	ŢB	�wB	�dB	�XB	�RB	�LB	�FB	�9B	�'B	�!B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�B	�B	~�B	{�B	y�B	y�B	x�B	x�B	x�B	v�B	t�B	p�B	iyB	cTB	_;B	ZB	S�B	P�B	O�B	M�B	J�B	I�B	H�B	E�B	@�B	;dB	6FB	33B	1'B	.B	)�B	%�B	"�B	!�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	oB	\B	PB	
=B		7B	1B	+B	%B	B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�sB�sB�sB�sB�sB�yB�sB�B�B�B�B�B�B�B�B�B�yB�yB�sB�sB�fB�mB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	  B	  B	B		7B	
=B	PB	VB	\B	bB	bB	hB	oB	oB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	#�B	%�B	&�B	(�B	(�B	)�B	)�B	)�B	)�B	-B	/B	0!B	0!B	1'B	1'B	1'B	5?B	6FB	7LB	8RB	9XB	9XB	:^B	;dB	<jB	=qB	=qB	A�B	E�B	F�B	G�B	H�B	H�B	F�B	E�B	D�B	D�B	F�B	J�B	K�B	N�B	R�B	S�B	T�B	[#B	^5B	bNB	dZB	e`B	e`B	gmB	gmB	hsB	k�B	l�B	n�B	y�B	y�B	y�B	z�B	�B	�+B	�1B	�7B	�DB	�JB	�JB	�VB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�3B	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�XB	�XB	�^B	�dB	�jB	�jB	�jB	�jB	�}B	�}B	��B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�;B	�B	��B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B

=B
DB
PB
bB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
(�B
,B
-B
.B
/B
0!B
0!B
1'B
1'B
33B
7LB
9XB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
A�B
C�B
C�B
D�B
E�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
Q�B
R�B
S�B
T�B
T�B
T�B
W
B
XB
XB
YB
[#B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
`BB
aHB
bNB
cTB
dZB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
hsB
hsB
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
l�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
t�B
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
x�B
x�B
x�B
y�B
y�B
y�B
z�B
|�B
}�B
}�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�DB
�DB
�PB
�PB
�\B
�\B
�bB
�bB
�bB
�bB
�\B
�\B
�bB
�\B
�\B
�bB
�bB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�3B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�qB
�qB
�qB
�wB
�wB
�qB
�qB
�wB
�}B
�}B
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
��B
��B
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
B
B
B
B
B
B
B
B
B
B
ÖB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ǮB
ǮB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
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
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
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
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�/B
�5B
�5B
�5B
�5B
�5B
�5B
�54444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B��B��B�B�B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�-B�3B�3B�9B�9B�9B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�?B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�?B�FB�^B�XB�dB�qBĜBƨBƨBɺB��B��B�;B�TB�AB�5B�#B��B��B��B��B��B��B��BɺBɺBɺBȴBǮBƨBŢBĜBÕBB�wB�dB�FB�B��B��B�JB}�BiyBT�BK�BD�B=qB33B/B+B�B��B�B�fB�NB�;B�/B�#B�B�B�B��B��B�}B��B��B��B��B�VB�1B�Bp�BXB>wB"�B\B
��B
�B
�B
�HB
��B
��B
��B
ŢB
�jB
�?B
�B
��B
��B
�\B
�B
z�B
n�B
_;B
Q�B
H�B
@�B
6FB
,B
�B
�B
\B

=B
B	�B	�;B	�)B	��B	��B	�FB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�VB	�B	z�B	s�B	iyB	bNB	_;B	]/B	\)B	[#B	ZB	XB	T�B	S�B	Q�B	N�B	K�B	J�B	I�B	H�B	G�B	D�B	@�B	:^B	2-B	(�B	&�B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	PB	+B	B��B��B��B�B�B�B�B�B�yB�ZB�;B�B�
B��B��B��BɺBƨBŢBĜBÕBÕBB�wB�jB�jB�jB�jB�jB�dB�dB�^B�XB�LB�FB�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�bB�\B�VB�JB�JB�JB�JB�JB�PB�JB�VB�VB�VB�VB�VB�VB�VB�VB�VB�PB�PB�JB�JB�=B�DB�JB�VB�bB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�uB�uB�{B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�9B�9B�?B�FB�FB�LB�XB�dB�wB�}B��BBÕBŢBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�)B�/B�/B�5B�;B�AB�HB�HB�`B�yB�B�B�B�B�B�yB�sB�sB�B�B�B�B��B��B��B��B	B	%B	1B		7B		7B	DB	DB	JB	\B	bB	oB	�B	�B	�B	�B	$�B	+B	,B	-B	/B	0!B	0!B	2-B	2-B	33B	5?B	6FB	7LB	9XB	C�B	F�B	H�B	K�B	N�B	Q�B	S�B	T�B	T�B	T�B	W
B	YB	YB	ZB	ZB	[#B	[#B	[#B	]/B	]/B	^5B	_;B	`AB	`AB	`AB	`AB	cTB	cTB	dZB	dZB	dZB	dZB	ffB	ffB	gmB	hsB	iyB	jB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	s�B	t�B	u�B	u�B	v�B	z�B	}�B	�B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�dB	�qB	�wB	�}B	��B	��B	��B	��B	B	ÕB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�/B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
!�B
!�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
1'B
1'B
33B
33B
49B
49B
49B
49B
33B
33B
49B
33B
33B
49B
49B
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
7LB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
=qB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
H�B
H�B
H�B
I�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
[#B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
aHB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
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
t�B
t�B
t�B
u�B
u�B
v�B
w�B
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
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
|�B
|�B
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
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O� # 2 ! "                                                                ' &                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1 G0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0900000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 05 10 2018 117 -0.0900000 0.0040 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                        20230608223110              20230608223110              AO  ARCAADJP                                                                    20230608223110    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230608223110    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230608223110  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230608223110  QCF$                G�O�G�O�G�O�8000            