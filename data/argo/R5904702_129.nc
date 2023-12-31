CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-08T22:31:09Z creation      
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
_FillValue                 �  K|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ޼   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20230608223109  20230608223109  5904702 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  5449                            2B  A   NAVIS_A                         0470                            011514                          863 @ؤ�a�1   @ؤ���B.@8��n���d�1&�y1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F       @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@P  @�  @�33A��A%��AE��Ae��A���A���A���A���A���A���A�  A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B��fB�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�  C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2�D2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDufDu�fDvfDv�fDwfDw�fDxfDx�fDyfDy�fDzfDz�fD{fD{�fD|fD|�fD}fD}�fD~fD~�fDfD�fD�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D�� D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�NfD��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D3D��3D�3D�K3DË3D��3D�3D�K3Dċ3D��3D�3D�K3Dŋ3D��3D�3D�K3DƋ3D��3D�3D�K3Dǋ3D��3D�3D�K3Dȋ3D��3D�3D�K3Dɋ3D��3D�3D�K3Dʋ3D��3D�3D�K3Dˋ3D��3D�3D�K3D̋3D��3D�3D�K3D͋3D��3D�3D�K3D΋3D��3D�3D�K3Dϋ3D��3D�3D�K3DЋ3D��3D�3D�K3Dы3D��3D�3D�K3Dҋ3D��3D�3D�K3DӋ3D��3D�3D�K3Dԋ3D��3D�3D�K3DՋ3D��3D�3D�K3D֋3D��3D�3D�K3D׋3D��3D�3D�K3D؋3D��3D�3D�K3Dً3D��3D�3D�K3Dڋ3D��3D�3D�K3Dۋ3D��3D�3D�K3D܋3D��3D�3D�K3D݋3D��3D�3D�K3Dދ3D��3D�3D�K3Dߋ3D��3D�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D� D�K3D�3D��3D�3D�NfD�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�NfD�fD��fD�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A�  A�  A���A���A���A���A���A���A���A��A��mA��^A��+A�ZA��yA��+A��A�^5A��9A���A��PA�dZA�?}A�E�A��#A�ƨA�Q�A��A��A���A��FA�n�A�ĜA�ffA��HA�A�A��HA��FA�&�A���A��A� �A�
=A���A�ĜA��9A��-A��7A� �A�{A�{A���A���A��hA�hsA�5?A�E�A���A�|�A�p�A�(�A�v�A�
=A���A�K�A���A��7A�?}A���A��jA�n�A�1'A�oA��/A�9XA��A��A�ĜA�A�A�K�A�ĜA�%A�ƨA���A�%A�l�A�hsA��A�n�A���A��^A��-A���A�dZA�oA�|�A�ffA�C�A�&�A��A��HA��A�?}A���A�;dA�~�A�ȴA�5?A�bNA�r�A�$�A��A��jA�TA}oA|�DA{�PAz�uAx��AxVAv�At�Ar�AqdZAp-Ao\)AnȴAnJAml�Alv�Ak�^Ak/Ai�Ah-AeO�Abv�A_�A^1'A\�/A[�
A[|�A[K�A[%AZ(�AY��AW�AT�AQ"�AP�AM�-AL �AIl�AHn�AHQ�AHAG��AG�7AGt�AFbNAE�;AE��AE+AC�PABQ�AA\)A?�A?x�A?C�A>�A>~�A>-A=7LA;�
A;XA:jA9`BA8�yA8��A7�FA6��A6-A5�PA4��A41'A3��A2��A2{A1�
A1�;A1�mA1��A.��A-�mA-�A-l�A,z�A+��A+�A+C�A+&�A)t�A'��A'
=A&jA%��A%�7A%G�A$��A$VA#�;A#/A"�HA"�+A"ZA!x�A A�A�A�`AA�A��A�A%A�DAA��AbNA`BAA�HA5?A�A-A�A`BA��AhsAG�A
��A
r�A	7LAA�FA�hA�7Ax�Al�AhsA;dA^5Ar�A��A��Ax�A {@��F@�ff@���@��@���@�Q�@���@�&�@�o@��@���@���@�F@�t�@��@�{@�G�@��@�Q�@���@���@��@���@�+@�$�@��@��@�@���@�@�@�t�@�+@�^@�  @ݙ�@�&�@��@�+@�n�@�r�@�=q@�ƨ@�33@ҸR@��T@У�@�C�@́@̓u@�I�@�  @ʧ�@�x�@�V@��@ȼj@Ȭ@ț�@ȓu@ȋD@�b@�dZ@�V@ź^@�z�@�
=@��@��m@��\@���@�v�@��^@�G�@��@��;@�33@��@���@��@�/@�dZ@�-@�(�@�33@�M�@��@�9X@�S�@���@��+@��m@�C�@�@���@���@�O�@�z�@��@�dZ@�K�@�33@��y@�n�@�b@��m@��
@���@���@��@��h@�/@�z�@�1'@�b@�
=@���@�G�@��@���@�A�@�1'@�b@���@���@��@��@�ƨ@��!@�v�@�n�@�V@�5?@�$�@�$�@�$�@�{@���@�X@��@� �@��;@��@�l�@��@��@�V@��T@�O�@���@�;d@�
=@��@���@�M�@��@�x�@���@�bN@���@�33@��y@�ȴ@��!@���@��+@�v�@�ff@�ff@�^5@�M�@�M�@�V@�ff@��+@���@���@��R@���@���@��@���@��R@�~�@�V@�M�@��@�@�G�@��`@���@�j@�1'@��
@�dZ@��H@���@��+@�n�@�@��@�p�@�`B@�O�@�/@��/@�z�@� �@~ȴ@}O�@|��@|��@|�@|Z@{��@{��@{�@{S�@{@z�@z�@z�@z�@z�!@zn�@z^5@zM�@y�@y�@y��@yG�@x��@x�u@x�u@x�u@xr�@v��@vV@u��@t�@s�
@s��@sS�@s33@r�!@r-@q��@q�7@q&�@q�@pb@o;d@n�@nE�@mO�@k�m@j�\@i�#@i��@ihs@iX@i7L@h��@hb@g�;@g��@g��@g�w@g�P@g;d@f��@f��@fv�@fV@f@e?}@d�@d�/@d�@c��@c��@ct�@cS�@cC�@co@c@b�@a�^@a��@aX@a7L@`��@`��@`bN@`1'@_�@_��@_�P@_|�@_l�@_;d@_�@_
=@^��@^ȴ@^��@^v�@^E�@]��@]@]�h@]O�@]?}@\�/@\z�@\9X@\�@[�
@Z��@Z-@Y�^@Y�7@YG�@Y%@X��@XbN@W�;@W|�@W+@V��@V5?@V@U��@U�-@U`B@T�@Tj@T�@S�
@S�F@S��@S�@SS�@So@R�@R��@RM�@Q��@QX@P1'@N�@N�+@N5?@M�T@M��@L�@K�m@Kt�@K33@J��@Jn�@I�@I��@Ihs@H��@H �@Gl�@Fȴ@Fff@F$�@E/@D��@D�@Dz�@DI�@D(�@C�F@Ct�@B�@A�#@@��@@��@@Ĝ@@�@@bN@@Q�@@1'@@b@@b@?��@?
=@>��@>�y@>�R@>�+@>v�@>v�@>v�@>v�@>v�@>ff@>V@=�-@=?}@<��@<�D@<Z@<9X@;��@;ƨ@;�F@;�F@;��@;��@;��@;�@;t�@;dZ@;dZ@;S�@;S�@;S�@;33@;o@:��@:n�@9x�@8�`@8bN@7+@6��@6ff@65?@6@5�T@5�-@5`B@4�/@3�
@3dZ@333@3o@2n�@1�^@1�7@1hs@1G�@1&�@1�@1%@1%@1%@1%@1%@0��@0�`@0�u@0bN@0b@/�w@/�P@/
=@.�@.��@.�+@.5?@-�@-��@-��@-�@-?}@-O�@-V@,�D@,Z@,j@,I�@+33@*��@*J@)��@)��@)��@)�7@)X@)G�@)&�@(��@'�w@'K�@';d@';d@';d@'+@'
=@&V@%��@%@%��@%��@%�T@%�@%�T@%�@%�@%�@%�T@%`B@$�/@$��@$��@$�j@$�@#ƨ@#t�@#o@"�\@!��@ Q�@+@�R@�+@E�@�T@p�@��@�j@��@9X@9X@��@ƨ@ƨ@�@S�@"�@��@�!@�!@~�@�7@�@�@bN@�@K�@�@��@�@�@�+@ff@{@�@�T@p�@/@V@�@��@�@��@�D@�D@�D@z�@z�@z�@z�@I�@9X@�@�
@��@��@��@��@��@t�@S�@C�@33@"�@o@o@��@��@��@�!@��@�!@�!@�!@��@~�@~�@�\@M�@-@�@J@hs@�@1'@ �@  @�@l�@��@ȴ@��@v�@v�@V@5?@{@�T@��@�@`B@?}@�@��@��@z�@9X@9X@(�@(�@(�@�@1@ƨ@�F@dZ@C�@33@"�@o@o@
�@
�H@
��@
�!@
��@
��@
�\@
M�@
=q@	��@	�#@	��@	��@	�^@	��@	�7@	X@	7L@	%@��@��@r�@r�@r�@Q�@A�@b@  @�@�@�@�@�@�@�;@�@�@�@�@�@��@|�@l�@;d@+@��@��@��@��@��@��@�h@V@�@�/@�j@��@j@9X@�@��@�
@�F@dZ@33@o@o@o@o@o@o@@�@�H@��@�!@��@~�@^5@-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A�  A�  A���A���A���A���A���A���A���A��A��mA��^A��+A�ZA��yA��+A��A�^5A��9A���A��PA�dZA�?}A�E�A��#A�ƨA�Q�A��A��A���A��FA�n�A�ĜA�ffA��HA�A�A��HA��FA�&�A���A��A� �A�
=A���A�ĜA��9A��-A��7A� �A�{A�{A���A���A��hA�hsA�5?A�E�A���A�|�A�p�A�(�A�v�A�
=A���A�K�A���A��7A�?}A���A��jA�n�A�1'A�oA��/A�9XA��A��A�ĜA�A�A�K�A�ĜA�%A�ƨA���A�%A�l�A�hsA��A�n�A���A��^A��-A���A�dZA�oA�|�A�ffA�C�A�&�A��A��HA��A�?}A���A�;dA�~�A�ȴA�5?A�bNA�r�A�$�A��A��jA�TA}oA|�DA{�PAz�uAx��AxVAv�At�Ar�AqdZAp-Ao\)AnȴAnJAml�Alv�Ak�^Ak/Ai�Ah-AeO�Abv�A_�A^1'A\�/A[�
A[|�A[K�A[%AZ(�AY��AW�AT�AQ"�AP�AM�-AL �AIl�AHn�AHQ�AHAG��AG�7AGt�AFbNAE�;AE��AE+AC�PABQ�AA\)A?�A?x�A?C�A>�A>~�A>-A=7LA;�
A;XA:jA9`BA8�yA8��A7�FA6��A6-A5�PA4��A41'A3��A2��A2{A1�
A1�;A1�mA1��A.��A-�mA-�A-l�A,z�A+��A+�A+C�A+&�A)t�A'��A'
=A&jA%��A%�7A%G�A$��A$VA#�;A#/A"�HA"�+A"ZA!x�A A�A�A�`AA�A��A�A%A�DAA��AbNA`BAA�HA5?A�A-A�A`BA��AhsAG�A
��A
r�A	7LAA�FA�hA�7Ax�Al�AhsA;dA^5Ar�A��A��Ax�A {@��F@�ff@���@��@���@�Q�@���@�&�@�o@��@���@���@�F@�t�@��@�{@�G�@��@�Q�@���@���@��@���@�+@�$�@��@��@�@���@�@�@�t�@�+@�^@�  @ݙ�@�&�@��@�+@�n�@�r�@�=q@�ƨ@�33@ҸR@��T@У�@�C�@́@̓u@�I�@�  @ʧ�@�x�@�V@��@ȼj@Ȭ@ț�@ȓu@ȋD@�b@�dZ@�V@ź^@�z�@�
=@��@��m@��\@���@�v�@��^@�G�@��@��;@�33@��@���@��@�/@�dZ@�-@�(�@�33@�M�@��@�9X@�S�@���@��+@��m@�C�@�@���@���@�O�@�z�@��@�dZ@�K�@�33@��y@�n�@�b@��m@��
@���@���@��@��h@�/@�z�@�1'@�b@�
=@���@�G�@��@���@�A�@�1'@�b@���@���@��@��@�ƨ@��!@�v�@�n�@�V@�5?@�$�@�$�@�$�@�{@���@�X@��@� �@��;@��@�l�@��@��@�V@��T@�O�@���@�;d@�
=@��@���@�M�@��@�x�@���@�bN@���@�33@��y@�ȴ@��!@���@��+@�v�@�ff@�ff@�^5@�M�@�M�@�V@�ff@��+@���@���@��R@���@���@��@���@��R@�~�@�V@�M�@��@�@�G�@��`@���@�j@�1'@��
@�dZ@��H@���@��+@�n�@�@��@�p�@�`B@�O�@�/@��/@�z�@� �@~ȴ@}O�@|��@|��@|�@|Z@{��@{��@{�@{S�@{@z�@z�@z�@z�@z�!@zn�@z^5@zM�@y�@y�@y��@yG�@x��@x�u@x�u@x�u@xr�@v��@vV@u��@t�@s�
@s��@sS�@s33@r�!@r-@q��@q�7@q&�@q�@pb@o;d@n�@nE�@mO�@k�m@j�\@i�#@i��@ihs@iX@i7L@h��@hb@g�;@g��@g��@g�w@g�P@g;d@f��@f��@fv�@fV@f@e?}@d�@d�/@d�@c��@c��@ct�@cS�@cC�@co@c@b�@a�^@a��@aX@a7L@`��@`��@`bN@`1'@_�@_��@_�P@_|�@_l�@_;d@_�@_
=@^��@^ȴ@^��@^v�@^E�@]��@]@]�h@]O�@]?}@\�/@\z�@\9X@\�@[�
@Z��@Z-@Y�^@Y�7@YG�@Y%@X��@XbN@W�;@W|�@W+@V��@V5?@V@U��@U�-@U`B@T�@Tj@T�@S�
@S�F@S��@S�@SS�@So@R�@R��@RM�@Q��@QX@P1'@N�@N�+@N5?@M�T@M��@L�@K�m@Kt�@K33@J��@Jn�@I�@I��@Ihs@H��@H �@Gl�@Fȴ@Fff@F$�@E/@D��@D�@Dz�@DI�@D(�@C�F@Ct�@B�@A�#@@��@@��@@Ĝ@@�@@bN@@Q�@@1'@@b@@b@?��@?
=@>��@>�y@>�R@>�+@>v�@>v�@>v�@>v�@>v�@>ff@>V@=�-@=?}@<��@<�D@<Z@<9X@;��@;ƨ@;�F@;�F@;��@;��@;��@;�@;t�@;dZ@;dZ@;S�@;S�@;S�@;33@;o@:��@:n�@9x�@8�`@8bN@7+@6��@6ff@65?@6@5�T@5�-@5`B@4�/@3�
@3dZ@333@3o@2n�@1�^@1�7@1hs@1G�@1&�@1�@1%@1%@1%@1%@1%@0��@0�`@0�u@0bN@0b@/�w@/�P@/
=@.�@.��@.�+@.5?@-�@-��@-��@-�@-?}@-O�@-V@,�D@,Z@,j@,I�@+33@*��@*J@)��@)��@)��@)�7@)X@)G�@)&�@(��@'�w@'K�@';d@';d@';d@'+@'
=@&V@%��@%@%��@%��@%�T@%�@%�T@%�@%�@%�@%�T@%`B@$�/@$��@$��@$�j@$�@#ƨ@#t�@#o@"�\@!��@ Q�@+@�R@�+@E�@�T@p�@��@�j@��@9X@9X@��@ƨ@ƨ@�@S�@"�@��@�!@�!@~�@�7@�@�@bN@�@K�@�@��@�@�@�+@ff@{@�@�T@p�@/@V@�@��@�@��@�D@�D@�D@z�@z�@z�@z�@I�@9X@�@�
@��@��@��@��@��@t�@S�@C�@33@"�@o@o@��@��@��@�!@��@�!@�!@�!@��@~�@~�@�\@M�@-@�@J@hs@�@1'@ �@  @�@l�@��@ȴ@��@v�@v�@V@5?@{@�T@��@�@`B@?}@�@��@��@z�@9X@9X@(�@(�@(�@�@1@ƨ@�F@dZ@C�@33@"�@o@o@
�@
�H@
��@
�!@
��@
��@
�\@
M�@
=q@	��@	�#@	��@	��@	�^@	��@	�7@	X@	7L@	%@��@��@r�@r�@r�@Q�@A�@b@  @�@�@�@�@�@�@�;@�@�@�@�@�@��@|�@l�@;d@+@��@��@��@��@��@��@�h@V@�@�/@�j@��@j@9X@�@��@�
@�F@dZ@33@o@o@o@o@o@o@@�@�H@��@�!@��@~�@^5@-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB�dB�dB�^B�^B�dB�dB�dB�^B�^B�^B�^B�^B�^B�^B�^B�^B�^B�^B�^B�dB�dB�dB�dB�^B�^B�^B�jB�jB��BƨB��B�B�;B�fB�B��BBB�B!�B�B�B�B"�B$�B(�B+B+B1'B7LB9XB7LB49B2-B1'B-B/B6FB6FB6FB49B1'B6FBA�BG�BN�BR�B\)B`BBP�BJ�BL�BL�BI�BZBe`BhsBiyBT�BB��B��B��B��B��B��B�VB�DB�=B�=B�7B�B}�Bx�Bp�BdZB[#BS�BB�B49B0!B�B�BPB1BB��B��B��B�B�BÖB�jB�dB�XB�LB�?B�B��B�=B{�Bl�BcTBZBP�BE�B;dB6FB/B"�B�B1BB
��B
��B
�B
�`B
�)B
��B
��B
�^B
�9B
�B
��B
��B
��B
��B
��B
�hB
�7B
|�B
k�B
YB
J�B
@�B
9XB
49B
2-B
0!B
-B
'�B
"�B
�B
+B	��B	�B	�mB	�5B	��B	��B	��B	��B	ȴB	ǮB	ƨB	��B	�wB	�jB	�XB	�-B	�B	��B	��B	��B	��B	��B	��B	��B	�hB	�=B	�+B	�B	|�B	z�B	w�B	s�B	o�B	l�B	jB	ffB	dZB	aHB	^5B	\)B	\)B	\)B	ZB	W
B	P�B	L�B	K�B	I�B	E�B	B�B	@�B	?}B	>wB	;dB	49B	0!B	-B	.B	-B	,B	)�B	(�B	'�B	$�B	"�B	 �B	�B	�B	�B	oB	\B	PB	JB		7B	B	B��B��B�B�B�B�B�mB�ZB�ZB�`B�TB�BB�;B�5B�)B�B�B�B��B��B��B��B��B��B��B��B��B��BɺBǮBƨBƨBƨBŢBĜBÖBB��B��B��B�}B�}B�}B�}B�}B�wB�wB�wB�wB�qB�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBBÖBĜBŢBȴBǮBǮBȴBȴB��B��B��B�B�B�#B�#B�BB�`B�mB�mB�sB�sB�sB�sB�sB�mB�mB�mB�mB�sB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	1B		7B		7B	hB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	(�B	(�B	(�B	(�B	)�B	,B	-B	-B	/B	/B	/B	49B	<jB	=qB	>wB	?}B	B�B	B�B	B�B	C�B	F�B	P�B	R�B	S�B	ZB	[#B	\)B	\)B	]/B	]/B	]/B	]/B	]/B	]/B	aHB	hsB	k�B	o�B	r�B	u�B	x�B	x�B	z�B	}�B	�B	�%B	�\B	�bB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�XB	�qB	�wB	�wB	�wB	�}B	B	ĜB	ŢB	ŢB	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�;B	�;B	�;B	�;B	�BB	�ZB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
JB
PB
VB
bB
uB
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
#�B
%�B
%�B
'�B
'�B
(�B
)�B
,B
-B
-B
1'B
33B
33B
33B
49B
49B
5?B
5?B
8RB
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
F�B
G�B
H�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
R�B
S�B
T�B
T�B
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
W
B
XB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
bNB
cTB
cTB
cTB
dZB
e`B
ffB
ffB
gmB
ffB
hsB
hsB
iyB
jB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
t�B
u�B
u�B
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
y�B
y�B
z�B
z�B
{�B
|�B
}�B
}�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�PB
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
�bB
�hB
�hB
�hB
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�dB
�dB
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�}B
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
B
B
B
B
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
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
��44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B_;B_;B_;B^5B^5B_;B_;B_;B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B_;B_;B_;B_;B^5B^5B^5B`AB`ABe`BjBn�By�B�B�=B��B��B��B��B�wBŢB��BÕBBƨBȴB��B��B��B��B�#B�/B�#B�B�B��B��B��B�B�B�B�B��B�B�`B�B�B��B  BB��B�B�B�B�B��B	7BJBPB��BffBL�BI�BF�BB�B@�B9XB2-B/B.B.B-B'�B!�B�B{B1B��B��B�fB�B��BB�dB�'B�B��B��B��B��B�bB}�BgmB`AB_;B]/B[#BYBQ�B=qB.B�BbB+B
��B
��B
�yB
�;B
�B
��B
ƨB
�dB
�B
��B
��B
��B
�\B
�7B
� B
o�B
e`B
^5B
XB
Q�B
M�B
H�B
D�B
>wB
9XB
5?B
-B
 �B
\B	��B	�B	�ZB	�/B	�B	�B	��B	��B	��B	ƨB	�jB	�B	��B	�{B	�DB	�B	u�B	q�B	p�B	n�B	l�B	k�B	jB	e`B	bNB	`AB	]/B	VB	O�B	J�B	D�B	B�B	@�B	>wB	<jB	:^B	5?B	.B	+B	%�B	 �B	�B	�B	�B	uB	bB	VB	
=B	1B	B	B	  B	  B	  B��B��B��B�B�B�B�yB�fB�ZB�TB�NB�;B�B��B��B��B��B��B��B��B��BȴBƨBĜB��B�qB�XB�FB�3B�'B�!B�B��B��B��B��B�hB�bB�\B�VB�DB�1B�1B�7B�+B�B�B�B� B|�By�By�Bx�Bx�Bx�Bx�Bx�Bw�Bu�Bs�Bq�Bo�Bm�Bk�BjBjBjBiyBhsBgmBffBe`BdZBdZBcTBcTBcTBcTBcTBbNBbNBbNBbNBaHB`ABdZBdZBe`Be`Be`Be`BdZBdZBe`BdZBe`Be`Be`Be`Be`BffBffBgmBhsBiyBl�Bk�Bk�Bl�Bl�Bn�Br�Bx�B|�B}�B~�B~�B�B�7B�DB�DB�JB�JB�JB�JB�JB�DB�DB�DB�DB�JB�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�RB�RB�dB�dB�jB�wBBBBBBÕB��B��B��B��B��B��B��B��B��B��B��B�B�AB�HB�NB�TB�fB�fB�fB�mB�B��B��B��B��B��B	  B	  B	B	B	B	B	B	B	B	JB	\B	uB	�B	�B	�B	�B	�B	!�B	%�B	)�B	33B	49B	49B	49B	5?B	7LB	9XB	@�B	E�B	H�B	G�B	G�B	H�B	I�B	I�B	K�B	K�B	K�B	K�B	K�B	K�B	L�B	L�B	M�B	N�B	O�B	O�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	Q�B	Q�B	Q�B	P�B	P�B	Q�B	Q�B	R�B	S�B	T�B	W
B	YB	[#B	\)B	]/B	]/B	aHB	bNB	bNB	bNB	cTB	ffB	hsB	iyB	iyB	r�B	v�B	w�B	x�B	y�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�1B	�VB	�VB	�\B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�LB	�XB	�dB	�jB	�jB	�jB	�qB	��B	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�;B	�AB	�AB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B

=B

=B
DB

=B
JB
JB
PB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
(�B
)�B
)�B
)�B
+B
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
/B
/B
/B
0!B
1'B
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
33B
33B
49B
5?B
5?B
5?B
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
@�B
A�B
A�B
A�B
B�B
A�B
B�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
H�B
J�B
J�B
K�B
K�B
K�B
L�B
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
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
R�B
T�B
T�B
T�B
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
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
_;B
_;B
_;B
_;B
`AB
`AB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
ffB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
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
r�B
r�B
r�B
r�B
s�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                      !        !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ' q00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            surface_pressure=-0.35 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0900000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 05 10 2018 117 -0.0900000 0.0040 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                        20230608223109              20230608223109              AO  ARCAADJP                                                                    20230608223109    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230608223109    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230608223109  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230608223109  QCF$                G�O�G�O�G�O�8000            