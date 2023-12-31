CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-03T21:36:35Z creation;2019-01-03T21:36:38Z conversion to V3.1;2019-12-23T06:09:14Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܘ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20190103213635  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  I2_0675_115                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؝/��A�1   @؝0>�� @8�E����c6 ѷY1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��A\)A(��AJ�\Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	�
B=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�:�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl�=Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du#�Du��Dv#�Dv��Dw#�Dw��Dx#�Dx��Dy#�Dy��Dz#�Dz��D{#�D{��D|#�D|�qD}#�D}��D~#�D~��D#�D��D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D�D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D��D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D�D���D��D�Q�DÑ�D���D��D�Q�Dđ�D���D��D�Q�Dő�D���D��D�Q�DƑ�D���D��D�Q�DǑ�D���D��D�Q�Dȑ�D���D��D�Q�Dɑ�D���D��D�Q�Dʑ�D���D��D�Q�Dˑ�D���D��D�Q�D̑�D���D��D�Q�D͑�D���D��D�Q�DΑ�D���D��D�Q�Dϑ�D���D��D�Q�DБ�D���D��D�Q�Dё�D���D��D�Q�Dґ�D���D��D�Q�Dӑ�D���D��D�Q�Dԑ�D���D��D�Q�DՑ�D���D��D�Q�D֑�D���D��D�Q�Dב�D���D��D�Q�Dؑ�D���D�D�Q�Dّ�D���D��D�Q�Dڑ�D���D��D�Q�Dۑ�D���D��D�Q�Dܑ�D���D��D�Q�Dݑ�D���D��D�Q�Dޑ�D���D��D�Q�Dߑ�D���D��D�Q�D���D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D�D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D��D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A�n�A�r�A�z�A�t�A�n�A�r�A�r�A�n�A��A�~�A�x�A�x�A�z�A�z�A��hA��A7A+A�\)A��A��yA�A���A�`BA�5?A�A��!A�XA��A��A�ZA�33A�$�A��A�oA��A���A���A���A���A���A��-A��hA�t�A�\)A�"�A���A�O�A���A�&�A�n�A�+A�jA���A��wA�-A�S�A�?}A�\)A�A�9XA���A�v�A���A���A��hA�Q�A��A��DA�7LA�M�A��^A�dZA�x�A�bA��PA�`BA�VA��A��DA�E�A�?}A��A��A��A�A�A���A�bA�$�A�C�A�5?A�XA�7LA���A�  A�z�A���A�bA���A���A�;dA�33A��uA�
=A�A���A�;A�A}�-A|M�Az��Awx�Au�ArE�ApE�Ao��An�9Ak�FAiC�Ad(�A_�wA]�;A]�hA]&�A\E�A\�A[��AZ1'AX �AWhsAU��AT(�AN�!AL�HAK��AJ�!AI��AH�AD�uACdZACXACS�AC"�AB�+AA�PA@�`A@ �A?�A?\)A=��A<v�A<JA;�^A;?}A:�RA9�
A9�A8�/A8�A81'A7S�A6bA4E�A3A1ƨA1hsA0�!A0  A.�A-��A,ȴA,A�A+l�A*M�A)�-A(Q�A&��A%?}A#x�A"��A!��A�A�A7LA�A�9A�^AJAjAl�An�A\)A"�AoA��A�A��A=qA�Ar�A��A=qA�^A��A��A~�A��A	�^A�uAO�AjA�A/A�
A~�A��AO�A ��@���@���@��@�t�@���@���@��;@���@�M�@��7@��
@�S�@���@�-@��/@�S�@���@�p�@�A�@�|�@��#@�w@�M�@��@�%@�Q�@�C�@��@�\@��@��/@ߕ�@݉7@��@�E�@ف@ו�@���@��@���@ԛ�@� �@��@�X@�"�@ͩ�@̛�@�\)@ɑh@���@���@�t�@�"�@���@��#@ă@�1@��m@���@å�@��H@�=q@�M�@�7L@�bN@�  @�o@��@��j@��@�+@��\@���@��@��u@��@�\)@�o@�E�@��T@�G�@���@��m@�dZ@���@�$�@��^@�hs@��@�I�@���@�t�@���@�{@�O�@��`@��j@��u@�j@��@�C�@���@�ff@�J@�x�@���@�j@�(�@��m@��@�C�@��@��R@�~�@�=q@���@�7L@��/@�j@��@���@�C�@�o@���@�@��#@���@�p�@���@��/@��j@��u@�z�@�1@���@���@���@�t�@�ȴ@���@�^5@�$�@�@��@���@��^@���@��@�p�@�&�@���@��@�S�@���@��\@�@���@��-@���@��h@�x�@�X@�&�@�V@�Ĝ@���@���@�bN@�b@��m@��w@���@��P@�t�@�K�@�o@���@��H@��!@�v�@�M�@�{@�@��7@�O�@�?}@�V@��u@�A�@��m@��@�;d@��@���@�^5@�E�@�=q@��@��h@���@��^@���@�x�@�O�@�7L@�V@��/@��9@��@�Q�@��@�  @���@���@���@�v�@�-@��@�~�@��!@�^5@�J@��7@��@�7L@��@��/@��j@��9@���@�r�@��@�1'@��;@��
@��@�;d@�
=@�C�@�C�@�;d@��H@�=q@��T@��h@�hs@��@��@��@�r�@�1'@�  @���@���@���@���@�S�@�@�v�@�v�@�$�@���@��^@��7@�`B@�?}@�?}@�/@�&�@���@��D@��@�Z@�1'@� �@��@�b@�  @��@��m@��;@���@��@���@�dZ@�\)@�n�@�5?@�E�@�V@�~�@��\@��\@�v�@�$�@��@��h@�G�@�/@�%@��/@�Ĝ@���@��u@�Q�@�(�@�@�@�P@�P@+@~��@~�@~�R@~��@~E�@~{@}�@}��@}��@}O�@|�@|�/@|�@|Z@{ƨ@{�@{S�@{@z�H@z~�@y�#@yx�@yX@y�@x�`@xr�@x  @w��@wK�@v�R@vE�@v$�@u�@u/@t��@t�/@t�j@tZ@s��@r�!@rJ@q�@q��@qX@q&�@p��@pĜ@pbN@pb@o|�@n��@n�R@n�+@nE�@m�@m�@mO�@m?}@m�@l�@l�D@lZ@lI�@l�@k�
@k��@k@j�!@i��@iX@i�@hĜ@hbN@g�;@g|�@g;d@f�y@f�+@f5?@f$�@e��@eO�@e/@e�@d�@d9X@c��@cdZ@co@b�!@b�@a�#@a��@ax�@a&�@a�@a%@`�`@`�u@`1'@_��@_l�@^��@^�R@^v�@^{@]��@]�h@]V@\(�@[��@[�@[S�@[@Z��@ZM�@Z�@Y�^@YX@XĜ@X�@X  @W�P@W;d@Vȴ@VE�@V$�@U��@U�@U/@T��@T�D@Tj@T9X@S��@Sƨ@S�F@S��@S�@S33@R��@R�\@RM�@Q�#@Q��@Q��@Q�7@Qx�@QX@QG�@Q7L@Q%@P��@P�9@PbN@P �@O�;@O�P@N�@Nȴ@N�R@N��@N�+@Nv�@Nff@N5?@M�@M?}@L��@L��@L��@L9X@Kƨ@K��@KC�@J�@J~�@J-@I�^@Ihs@IG�@I7L@I&�@H��@HA�@Hb@H  @G�;@G��@G|�@G\)@G;d@F��@F��@Fv�@F5?@F{@E��@E?}@D�@DZ@C�m@Ct�@C33@Co@C@B�\@B�@A��@A7L@@�@@  @?�w@?��@?|�@?
=@>��@>V@=�@=�h@=O�@=V@<��@<z�@<9X@<�@;�
@;t�@;C�@:�@:�H@:��@:~�@:M�@9�@9��@9G�@8��@8��@8�u@8�@8A�@8b@7��@7l�@6�y@6��@6��@6V@5@5�@5?}@5V@4�@4�j@4z�@4I�@4(�@3��@3�
@3ƨ@3��@3C�@2��@2~�@2-@1�#@1�^@1��@1�7@17L@1%@0�`@0�9@0��@0��@0�u@01'@/��@/��@/K�@.�@.�+@.v�@.ff@.$�@-��@-p�@-/@,�/@,�@,�D@,9X@+��@+�F@+��@+�@+C�@*�@*��@*^5@*-@*J@)�#@)��@)X@)7L@)�@(��@(Ĝ@(�@(Q�@( �@(  @'�w@'�P@'|�@';d@&��@&�@&�+@&5?@&@%@%�h@%V@$��@$�/@$�@$z�@$Z@$(�@$�@#�
@#S�@#@#@#@#@"�@"�@"�H@"�H@"��@"�!@"�\@"=q@!��@!�#@!��@!hs@!7L@!%@ ��@ �`@ ��@ Q�@�@��@|�@|�@\)@+@�@�@��@�R@{@�T@�h@?}@�/@�@��@z�@I�@�m@�F@�@dZ@o@o@@�@��@��@^5@-@��@��@hs@Ĝ@bN@Q�@Q�@A�@1'@ �@b@�;@�P@�y@��@�+@v�@5?@�T@��@��@�@p�@`B@O�@V@��@��@I�@��@�
@ƨ@�F@��@S�@@�!@~�@n�@M�@-@��@��@��@�7@�7@x�@&�@%@�`@��@bN@Q�@1'@ �@�@��@\)@�@��@�y@ȴ@��@��@V@5?@�T@�-@�@/@V@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A�n�A�r�A�z�A�t�A�n�A�r�A�r�A�n�A��A�~�A�x�A�x�A�z�A�z�A��hA��A7A+A�\)A��A��yA�A���A�`BA�5?A�A��!A�XA��A��A�ZA�33A�$�A��A�oA��A���A���A���A���A���A��-A��hA�t�A�\)A�"�A���A�O�A���A�&�A�n�A�+A�jA���A��wA�-A�S�A�?}A�\)A�A�9XA���A�v�A���A���A��hA�Q�A��A��DA�7LA�M�A��^A�dZA�x�A�bA��PA�`BA�VA��A��DA�E�A�?}A��A��A��A�A�A���A�bA�$�A�C�A�5?A�XA�7LA���A�  A�z�A���A�bA���A���A�;dA�33A��uA�
=A�A���A�;A�A}�-A|M�Az��Awx�Au�ArE�ApE�Ao��An�9Ak�FAiC�Ad(�A_�wA]�;A]�hA]&�A\E�A\�A[��AZ1'AX �AWhsAU��AT(�AN�!AL�HAK��AJ�!AI��AH�AD�uACdZACXACS�AC"�AB�+AA�PA@�`A@ �A?�A?\)A=��A<v�A<JA;�^A;?}A:�RA9�
A9�A8�/A8�A81'A7S�A6bA4E�A3A1ƨA1hsA0�!A0  A.�A-��A,ȴA,A�A+l�A*M�A)�-A(Q�A&��A%?}A#x�A"��A!��A�A�A7LA�A�9A�^AJAjAl�An�A\)A"�AoA��A�A��A=qA�Ar�A��A=qA�^A��A��A~�A��A	�^A�uAO�AjA�A/A�
A~�A��AO�A ��@���@���@��@�t�@���@���@��;@���@�M�@��7@��
@�S�@���@�-@��/@�S�@���@�p�@�A�@�|�@��#@�w@�M�@��@�%@�Q�@�C�@��@�\@��@��/@ߕ�@݉7@��@�E�@ف@ו�@���@��@���@ԛ�@� �@��@�X@�"�@ͩ�@̛�@�\)@ɑh@���@���@�t�@�"�@���@��#@ă@�1@��m@���@å�@��H@�=q@�M�@�7L@�bN@�  @�o@��@��j@��@�+@��\@���@��@��u@��@�\)@�o@�E�@��T@�G�@���@��m@�dZ@���@�$�@��^@�hs@��@�I�@���@�t�@���@�{@�O�@��`@��j@��u@�j@��@�C�@���@�ff@�J@�x�@���@�j@�(�@��m@��@�C�@��@��R@�~�@�=q@���@�7L@��/@�j@��@���@�C�@�o@���@�@��#@���@�p�@���@��/@��j@��u@�z�@�1@���@���@���@�t�@�ȴ@���@�^5@�$�@�@��@���@��^@���@��@�p�@�&�@���@��@�S�@���@��\@�@���@��-@���@��h@�x�@�X@�&�@�V@�Ĝ@���@���@�bN@�b@��m@��w@���@��P@�t�@�K�@�o@���@��H@��!@�v�@�M�@�{@�@��7@�O�@�?}@�V@��u@�A�@��m@��@�;d@��@���@�^5@�E�@�=q@��@��h@���@��^@���@�x�@�O�@�7L@�V@��/@��9@��@�Q�@��@�  @���@���@���@�v�@�-@��@�~�@��!@�^5@�J@��7@��@�7L@��@��/@��j@��9@���@�r�@��@�1'@��;@��
@��@�;d@�
=@�C�@�C�@�;d@��H@�=q@��T@��h@�hs@��@��@��@�r�@�1'@�  @���@���@���@���@�S�@�@�v�@�v�@�$�@���@��^@��7@�`B@�?}@�?}@�/@�&�@���@��D@��@�Z@�1'@� �@��@�b@�  @��@��m@��;@���@��@���@�dZ@�\)@�n�@�5?@�E�@�V@�~�@��\@��\@�v�@�$�@��@��h@�G�@�/@�%@��/@�Ĝ@���@��u@�Q�@�(�@�@�@�P@�P@+@~��@~�@~�R@~��@~E�@~{@}�@}��@}��@}O�@|�@|�/@|�@|Z@{ƨ@{�@{S�@{@z�H@z~�@y�#@yx�@yX@y�@x�`@xr�@x  @w��@wK�@v�R@vE�@v$�@u�@u/@t��@t�/@t�j@tZ@s��@r�!@rJ@q�@q��@qX@q&�@p��@pĜ@pbN@pb@o|�@n��@n�R@n�+@nE�@m�@m�@mO�@m?}@m�@l�@l�D@lZ@lI�@l�@k�
@k��@k@j�!@i��@iX@i�@hĜ@hbN@g�;@g|�@g;d@f�y@f�+@f5?@f$�@e��@eO�@e/@e�@d�@d9X@c��@cdZ@co@b�!@b�@a�#@a��@ax�@a&�@a�@a%@`�`@`�u@`1'@_��@_l�@^��@^�R@^v�@^{@]��@]�h@]V@\(�@[��@[�@[S�@[@Z��@ZM�@Z�@Y�^@YX@XĜ@X�@X  @W�P@W;d@Vȴ@VE�@V$�@U��@U�@U/@T��@T�D@Tj@T9X@S��@Sƨ@S�F@S��@S�@S33@R��@R�\@RM�@Q�#@Q��@Q��@Q�7@Qx�@QX@QG�@Q7L@Q%@P��@P�9@PbN@P �@O�;@O�P@N�@Nȴ@N�R@N��@N�+@Nv�@Nff@N5?@M�@M?}@L��@L��@L��@L9X@Kƨ@K��@KC�@J�@J~�@J-@I�^@Ihs@IG�@I7L@I&�@H��@HA�@Hb@H  @G�;@G��@G|�@G\)@G;d@F��@F��@Fv�@F5?@F{@E��@E?}@D�@DZ@C�m@Ct�@C33@Co@C@B�\@B�@A��@A7L@@�@@  @?�w@?��@?|�@?
=@>��@>V@=�@=�h@=O�@=V@<��@<z�@<9X@<�@;�
@;t�@;C�@:�@:�H@:��@:~�@:M�@9�@9��@9G�@8��@8��@8�u@8�@8A�@8b@7��@7l�@6�y@6��@6��@6V@5@5�@5?}@5V@4�@4�j@4z�@4I�@4(�@3��@3�
@3ƨ@3��@3C�@2��@2~�@2-@1�#@1�^@1��@1�7@17L@1%@0�`@0�9@0��@0��@0�u@01'@/��@/��@/K�@.�@.�+@.v�@.ff@.$�@-��@-p�@-/@,�/@,�@,�D@,9X@+��@+�F@+��@+�@+C�@*�@*��@*^5@*-@*J@)�#@)��@)X@)7L@)�@(��@(Ĝ@(�@(Q�@( �@(  @'�w@'�P@'|�@';d@&��@&�@&�+@&5?@&@%@%�h@%V@$��@$�/@$�@$z�@$Z@$(�@$�@#�
@#S�@#@#@#@#@"�@"�@"�H@"�H@"��@"�!@"�\@"=q@!��@!�#@!��@!hs@!7L@!%@ ��@ �`@ ��@ Q�@�@��@|�@|�@\)@+@�@�@��@�R@{@�T@�h@?}@�/@�@��@z�@I�@�m@�F@�@dZ@o@o@@�@��@��@^5@-@��@��@hs@Ĝ@bN@Q�@Q�@A�@1'@ �@b@�;@�P@�y@��@�+@v�@5?@�T@��@��@�@p�@`B@O�@V@��@��@I�@��@�
@ƨ@�F@��@S�@@�!@~�@n�@M�@-@��@��@��@�7@�7@x�@&�@%@�`@��@bN@Q�@1'@ �@�@��@\)@�@��@�y@ȴ@��@��@V@5?@�T@�-@�@/@V@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B(�B(�B(�B'�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�BQ�B�B)�B7LB6FB33B2-B2-B0!B-B'�B&�B.B1'B1'B5?B9XB8RB9XB9XB9XB9XB;dB=qB@�BB�BH�BM�BS�B\)B`BBffBjBq�Bp�Bv�B}�B�7B�%B�VB�DB�PB�\B�PB�\B�PB�PB�PB�DB�=B�7B�=B�=B�1B�1B�Bx�Bt�BffBR�BF�B?}B=qBB�BG�BD�B9XB-B%�B�B�BoBJBB��B�mB��B�B��B�hB�+Bs�BVB;dB)�B	7B
�HB
�wB
�B
��B
�VB
e`B
?}B
5?B
+B
�B
VB	��B	�`B	��B	�dB	�?B	�B	��B	�uB	t�B	Q�B	A�B	>wB	;dB	6FB	9XB	B�B	49B	#�B	1'B	'�B	�B�B�#B��B��BĜB�qB�RB�B��B��B�B�B�'B�LB�^B�RB�}BƨB�}B�qB�qB�XB�^B�RB�9B�-B�B�B��B��B��B��B��B��B�{B�bB�PB�1B�B|�By�Bu�Bt�Bn�Bm�BjBl�BdZBbNBcTBbNBaHB`BB_;B]/BXBR�BP�BM�BL�BK�BJ�BI�BG�BD�BC�BC�B?}B>wB:^B7LB33B2-B1'B/B.B,B,B'�B%�B%�B%�B%�B%�B$�B#�B$�B#�B#�B"�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B"�B!�B!�B!�B �B �B �B!�B"�B$�B&�B'�B'�B)�B)�B.B-B-B-B/B0!B33B5?B6FB6FB7LB6FB8RB8RB9XB:^B=qBA�BC�BC�BC�BC�BE�BG�BH�BN�BP�BQ�BT�BVBXBYB\)B]/B_;B`BBbNBe`BffBffBhsBl�Bp�Br�Bs�Bt�Bu�Bw�By�B{�B}�B� B�B�B�B�+B�=B�JB�PB�bB�oB��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�FB�^B�wBĜBǮB��B��B��B�
B�B�)B�HB�ZB�mB�B�B��B��B��B��B	B	B	B		7B	VB	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	%�B	'�B	+B	1'B	6FB	8RB	;dB	>wB	>wB	?}B	@�B	@�B	@�B	A�B	C�B	D�B	F�B	H�B	H�B	J�B	M�B	N�B	P�B	Q�B	R�B	S�B	T�B	W
B	W
B	XB	ZB	\)B	^5B	_;B	`BB	aHB	bNB	cTB	dZB	gmB	hsB	jB	l�B	k�B	jB	iyB	k�B	o�B	o�B	q�B	t�B	w�B	z�B	{�B	~�B	~�B	�B	�B	�B	�+B	�+B	�1B	�7B	�PB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�9B	�FB	�LB	�RB	�RB	�dB	��B	��B	��B	��B	��B	��B	ĜB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
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
JB
JB
JB
JB
PB
PB
JB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
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
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
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
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
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
9XB
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
=qB
=qB
=qB
>wB
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
@�B
A�B
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
D�B
D�B
D�B
D�B
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
F�B
G�B
G�B
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
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
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
S�B
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
ZB
[#B
[#B
[#B
[#B
[#B
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
aHB
bNB
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
hsB
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
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B(�B(�B(�B'�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�BQ�B�B)�B7B6B2�B1�B1�B/�B,�B'�B&�B-�B0�B0�B5B9$B8B9$B9$B9$B9$B;0B=<B@OBB[BH�BM�BS�B[�B`Bf2BjKBqvBpoBv�B}�B�B��B�"B�B�B�(B�B�(B�B�B�B�B�	B�B�	B�	B��B��B��Bx�Bt�Bf2BR�BFtB?HB=<BB[BGzBDgB9$B,�B%�BxBSB:BB�B�B�8BΥB��B�eB�4B��Bs�BU�B;0B)�B	B
�B
�BB
��B
��B
�"B
e,B
?HB
5B
*�B
�B
"B	��B	�,B	ҽB	�0B	�B	��B	��B	�@B	t�B	Q�B	AUB	>(B	;0B	6B	9$B	B[B	3�B	#�B	0�B	'�B	?B�UB��B��B̈́B�gB�<B�B��B��B��B��B��B��B�B�*B�B�HB�YB�HB�"B�<B�$B�*B�B��B��B��B��B��B��B�dB�KB�YB�2B�FB�B�B��B��B|�By�ButBtnBncBmCBjKBlWBd&BbBcBbB`�B_�B^�B\�BW�BR�BP�BM�BL~BKxBJrBIlBG_BDMBCGBCGB?.B>(B:B7B2�B1�B0�B.�B-�B+�B+�B'�B%�B%�B%�B%�B%�B$�B#�B$�B#�B#�B"�B"�B!|B!�B vB�BpB�B�BjBjBdBjB~BjB�B�BpB!|B#�B"�B!�B!|B!�B vB �B vB!|B"�B$�B&�B'�B'�B)�B)�B-�B,�B,�B,�B.�B/�B2�B4�B5�B6B6�B5�B8B8B9	B:B=<BA;BCGBCGBCGBCGBESBG_BHfBN�BP�BQ�BT�BU�BW�BX�B[�B\�B^�B_�Ba�BeBfBfBh>Bl=BpoBraBs�Bt�ButBw�By�B{�B}�B�B��B��B��B��B�	B��B�B�B�:B�2B�?B�WB�pB�vB��B��B��B��B��B��B��B��B��B�B�B�(B�gB�zB�rBΥBөBּB��B��B��B�B�B�KB�|B�nB��B��B��B	 �B	�B	�B	�B	B	FB	?B	kB	qB	xB	dB	 vB	!|B	"�B	$�B	%�B	'�B	*�B	0�B	5�B	8B	;B	>(B	>BB	?.B	@4B	@4B	@4B	AUB	CGB	DMB	FYB	H�B	HfB	JrB	M�B	N�B	P�B	Q�B	R�B	S�B	T�B	V�B	V�B	W�B	Y�B	[�B	^B	_B	_�B	`�B	bB	cB	dB	gB	h$B	j0B	l=B	k6B	j0B	i*B	k6B	oiB	oOB	qvB	tnB	w�B	z�B	{�B	~�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�:B	�,B	�2B	�9B	�YB	�KB	�dB	�dB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�4B	�;B	�;B	�;B	�;B	�4B	�MB	�YB	�tB	�YB	�_B	�rB	�xB	�xB	�~B	ϑB	ϑB	ЗB	ЗB	ЗB	ѝB	ԯB	өB	ּB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�$B	�0B	�0B	�0B	�6B	�CB	�cB	�IB	�IB	�iB	�OB	�UB	�[B	�[B	�aB	�aB	�aB	�hB	�B	�nB	�nB	�tB	�tB	�tB	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
	�B
	�B
	�B
�B
�B
�B
�B
B
B
�B
B
B
B
B
B
B
"B
"B
B
B
B
B
B
:B
 B
@B
&B
&B
,B
,B
FB
2B
2B
2B
9B
9B
9B
SB
SB
?B
EB
EB
EB
KB
eB
eB
QB
QB
QB
QB
kB
QB
WB
WB
WB
]B
]B
]B
dB
dB
~B
~B
jB
�B
�B
pB
pB
 vB
 �B
 vB
 vB
!|B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
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
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
4B
3�B
4�B
5B
4�B
5B
4�B
5�B
5�B
6B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9$B
9$B
:*B
;B
;B
;B
;B
<B
<B
<B
="B
="B
="B
>BB
>(B
>(B
>(B
>(B
?.B
?.B
?.B
@4B
@OB
@4B
@OB
@OB
AUB
A;B
AUB
BAB
B[B
BAB
BAB
BAB
BAB
CGB
CGB
DgB
DMB
DMB
DgB
ESB
EmB
EmB
EmB
ESB
FtB
FYB
FYB
FtB
FtB
FYB
FYB
FtB
GzB
G_B
G_B
HfB
HfB
HfB
HfB
HfB
H�B
H�B
I�B
I�B
IlB
IlB
IlB
I�B
J�B
JrB
JrB
KxB
K�B
KxB
K�B
KxB
KxB
L~B
L�B
L~B
L~B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
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
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\�B
\�B
^B
^B
]�B
^B
^B
]�B
]�B
]�B
^�B
_B
_B
^�B
^�B
^�B
_B
_�B
_�B
_�B
_�B
_�B
`�B
a�B
a�B
bB
a�B
a�B
bB
a�B
a�B
bB
c B
cB
cB
cB
dB
d&B
dB
dB
d&B
dB
dB
dB
eB
eB
eB
eB
f2B
fB
f2B
fB
f2B
f2B
fB
gB
gB
gB
gB
gB
h>B
h$B
h$B
h>B
h$B
h$B
h>B
h$B
i*B
i*B
i*B
i*B
i*B
iDB
iDB
j0B
jKB
jKB
j0B
jKB
j0B
j0B
k6B
kQB
k6B
kQB
kQB
lWB
l=B
l=B
lWB
mC1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.56(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901090040032019010900400320190109004003201901100025492019011000254920190110002549JA  ARFMdecpA19c                                                                20190104063627  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190103213635  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190103213636  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190103213636  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190103213637  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190103213637  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190103213637  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190103213637  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190103213637  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190103213638                      G�O�G�O�G�O�                JA  ARUP                                                                        20190103215610                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190104154313  CV  JULD            G�O�G�O�F��}                JM  ARCAJMQC2.0                                                                 20190108154003  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190108154003  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190109152549  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                