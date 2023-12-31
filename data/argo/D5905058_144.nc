CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-07T18:36:40Z creation;2019-05-07T18:36:44Z conversion to V3.1;2019-12-23T06:02:41Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190507183640  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_144                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؼ#���1   @ؼ$m�5 @7��i�B��c�J�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@�(�A
{A*{AJ{Aj{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �HC�HC�HC�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�C�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�C�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'��D((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt�RDu(RDu�RDv(RDv�RDw(RDw�RDx(RDx�RDy(RDy�RDz(RDz�RD{(RD{�RD|(RD|�RD}(RD}�RD~(RD~�RD(RD�RD�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D���D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D)D��)D�)D�T)DÔ)D��)D�)D�T)DĔ)D��)D�)D�T)DŔ)D��)D�)D�T)DƔ)D��)D�)D�T)Dǔ)D��)D�)D�T)DȔ)D��)D�)D�T)Dɔ)D��)D�)D�T)Dʔ)D��)D�)D�T)D˔)D��)D�)D�T)D̔)D��)D�)D�T)D͔)D��)D�)D�T)DΔ)D��)D�)D�T)Dϔ)D��)D�)D�T)DД)D��)D�)D�T)Dє)D��)D�)D�T)DҔ)D��)D�)D�T)DӔ)D��)D�)D�T)DԔ)D��)D�)D�T)DՔ)D��)D�)D�T)D֔)D��)D�)D�T)Dה)D��)D�)D�T)Dؔ)D��)D�)D�T)Dٔ)D��)D�)D�T)Dڔ)D��)D�)D�T)D۔)D��)D�)D�P�Dܔ)D��)D�)D�T)Dݔ)D��)D�)D�T)Dޔ)D��)D�)D�T)Dߔ)D��)D�)D�T)D��)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�\D�G\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��A���A�  A��A�A��jA��9A��A���A���A��A���A���A���A���A��A��A���A��uA�|�A�?}A��A���A��DA�5?A��`A��#A���A���A�~�A��A���A�VA��yA��!A�5?A�G�A� �A��A��#A�jA��/A�1'A�C�A��9A�hsA���A��A���A�  A��A��A�ffA�A�ƨA�`BA��wA��RA�p�A��A�bA���A���A�`BA�{A�%A���A�"�A�&�A���A��A�Q�A�A�`BA��A���A�7LA�5?A�1'A�+A��A�Q�A�t�A���A��A�dZA���A�t�A��wA�{A�jA�jA���A���A��mA��
A��A�1'A�v�A��HA��;A��uA��A�\)A��A���A��A�oA�M�A���A�~�A���A�VA�  A���A���A�|�A��;A}�-Az=qAu�Aq\)Am�
Am��Am�hAl��AjȴAi��Af��AeG�AdZAct�Aa��A_��A]��A]�A]+A]K�A]\)A]+A\ZA[;dAY�mAX��AW�PAUG�ATJARI�AQ��AP��AO33ALbAI�-AHffAG�AF �AD��ACC�AB^5AAC�A>�A=�TA>�A=�#A<��A:  A8�!A6��A5p�A4ffA2��A2bA1hsA0  A.�A.JA,�A+�#A*��A)�wA)dZA)�A'�A%�A$jA#dZA"1'A �A�A1AA�TA��AXA�HA��A�^A�AhsAZA��A�AI�A�A�yA��AVA|�A
�HA
�uA
{A	�TA	G�AbNA`BA33A�A=qA��Ap�A?}A�A��A�;A��AO�AC�A �y@��w@��+@�hs@�I�@�dZ@�=q@���@�|�@�@���@�w@�@�r�@�F@�ȴ@��`@�M�@��`@�9X@�@�v�@�u@� �@��@�l�@��y@���@�O�@��D@߾w@���@��@ޏ\@���@�bN@��@�@ش9@�b@��y@��@�7L@�t�@�9X@·+@�-@͡�@̋D@˝�@ʰ!@�p�@ȃ@Ǖ�@�E�@�hs@ēu@�t�@��@°!@�~�@�@�Ĝ@�9X@��F@��R@�G�@�Ĝ@���@���@��@�"�@�5?@���@�j@�9X@���@�t�@�+@�@�J@���@��@�?}@�&�@���@�t�@��H@���@��@�/@�bN@�t�@��@��H@��@��-@�9X@���@�-@���@��@�/@���@���@��R@��-@��`@��
@�K�@�ȴ@�^5@���@�@��h@��h@��h@�7L@���@�I�@��@��;@��F@���@�|�@�;d@��H@���@�{@��T@���@��^@���@�`B@��@���@�1'@���@���@�C�@��R@�V@��#@���@�`B@�/@��@�%@��/@��D@�I�@�1@���@�l�@�;d@�o@�~�@�-@�@��@��@��@��@��^@��7@�hs@�/@�/@��`@���@�I�@��@���@��@�t�@�K�@�;d@�o@�@���@���@�v�@�{@��T@���@���@�O�@��@�V@��@��u@�r�@�bN@��@��F@��@�|�@�\)@�S�@�"�@�@��@��y@���@��\@�v�@�5?@��@�@���@�x�@�hs@�G�@�&�@��@��@��@���@��/@���@�r�@�Z@�Z@�Q�@���@���@��@�l�@�S�@�@���@��\@�n�@�V@�J@��#@�@��7@��@���@���@��9@��u@�1'@���@�S�@�o@�o@��H@��!@�^5@�5?@�@���@�p�@��@��@���@��@�b@��@�K�@�@�@�ȴ@��R@���@�v�@��@�@���@��@��#@���@�`B@�&�@���@��@���@���@�r�@�A�@�1@l�@�P@;d@~E�@~5?@}��@}O�@|�@|�@|�@|Z@|1@{�F@{��@{��@{t�@{�@{S�@{"�@{33@z��@zM�@y�7@y7L@xĜ@x1'@wl�@w;d@v�@v�R@v��@v$�@u`B@u�@u�@uO�@tI�@t(�@t9X@t(�@s�
@s��@so@rM�@q��@q��@q&�@p�u@pb@o��@o\)@nȴ@n�+@nff@m�@n@m�h@mV@l�@l��@lz�@l(�@kƨ@k@jn�@j^5@j=q@i��@i�@h  @g
=@f��@fV@e�-@e�@ep�@e`B@e/@d�D@c�
@c"�@b��@b=q@a�@a�7@a7L@`�`@`r�@`  @_�w@_l�@^��@^5?@]p�@^��@]�@]��@^ff@^v�@^ff@]��@]p�@]O�@\��@\I�@[�
@["�@Z�H@ZM�@Y�7@Y&�@W��@W��@V��@VV@U�h@T�/@T�@S�
@Sƨ@Sƨ@S�F@S��@S�@S�@SdZ@SC�@R�@R��@R��@RM�@R=q@Q��@Qx�@Q�7@Q&�@P��@P�9@Pr�@P1'@O�P@O;d@O;d@O+@O+@N�y@N��@Nff@N{@N@M@M�h@Mp�@M/@L�@LI�@L1@K�m@Kƨ@Kƨ@Kƨ@Kƨ@K��@K33@Ko@K@J�H@J��@J�@I��@I&�@H��@HQ�@G�;@G�@G�P@GK�@G�@F�@Fȴ@F�+@F$�@E�@E��@E��@E�@EO�@EV@D�/@D�@DI�@D1@C��@C�m@Cƨ@Ct�@B�H@B~�@B^5@BM�@B=q@A��@A�^@@��@@r�@@ �@?�@?�@?�;@?�;@?�@?+@>ff@>$�@=��@=��@=p�@=O�@=�@<�@<j@<I�@<(�@<1@;��@;�
@;ƨ@;��@;�@;o@:��@:��@:�\@:^5@:M�@:J@9��@97L@9%@8��@8Ĝ@8b@7�;@7��@7�@7K�@7
=@6�@6�R@6v�@6E�@6$�@6@5��@5�h@5O�@4��@4�j@4�D@4Z@49X@4�@3ƨ@333@3@2�@2�H@2�!@2�!@2��@2M�@2�@2�@2J@1�@1�#@1�^@1��@1X@17L@17L@17L@1�@0�`@0�9@0�u@0bN@0A�@0 �@/�;@/�w@/��@/|�@/;d@/�@.�y@.ȴ@.��@.V@.5?@-�T@-@-�-@-`B@-?}@-�@-V@,��@,Z@+�
@+ƨ@+�@+33@*��@*^5@)�#@)��@)��@)X@)�@(Ĝ@(�u@(bN@(1'@(  @'�@'|�@'\)@'�@&�y@&�@&ȴ@&��@&��@&�+@&ff@&$�@%�@%`B@%V@$�/@$�@$�/@$�j@$z�@$I�@$(�@#��@#�
@#�F@#�@#t�@#"�@"�!@"�\@"n�@!�#@!�^@!��@!��@!hs@!&�@ ��@ �u@ bN@  �@��@K�@��@ȴ@v�@E�@@�@�@�-@O�@V@�@��@�D@I�@ƨ@�F@t�@"�@@�H@�!@^5@-@�@J@��@�#@�^@��@��@�7@hs@X@7L@&�@�@�9@A�@ �@  @��@�@|�@+@�y@�R@V@{@�@�T@`B@�@��@�j@z�@Z@�@��@�m@�
@ƨ@�F@��@��@�@S�@�@�H@��@��@=q@-@��@��@��@7L@��@�@A�@1'@b@��@��@l�@K�@;d@��@�@ȴ@�R@��@v�@V@@@�@�@�/@�D@(�@�
@ƨ@��@S�@"�@
�@
�!@
M�@
-@	�#@	��@	�7@	x�@	x�@	hs@	hs@	X@	X@	G�@	G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��A���A�  A��A�A��jA��9A��A���A���A��A���A���A���A���A��A��A���A��uA�|�A�?}A��A���A��DA�5?A��`A��#A���A���A�~�A��A���A�VA��yA��!A�5?A�G�A� �A��A��#A�jA��/A�1'A�C�A��9A�hsA���A��A���A�  A��A��A�ffA�A�ƨA�`BA��wA��RA�p�A��A�bA���A���A�`BA�{A�%A���A�"�A�&�A���A��A�Q�A�A�`BA��A���A�7LA�5?A�1'A�+A��A�Q�A�t�A���A��A�dZA���A�t�A��wA�{A�jA�jA���A���A��mA��
A��A�1'A�v�A��HA��;A��uA��A�\)A��A���A��A�oA�M�A���A�~�A���A�VA�  A���A���A�|�A��;A}�-Az=qAu�Aq\)Am�
Am��Am�hAl��AjȴAi��Af��AeG�AdZAct�Aa��A_��A]��A]�A]+A]K�A]\)A]+A\ZA[;dAY�mAX��AW�PAUG�ATJARI�AQ��AP��AO33ALbAI�-AHffAG�AF �AD��ACC�AB^5AAC�A>�A=�TA>�A=�#A<��A:  A8�!A6��A5p�A4ffA2��A2bA1hsA0  A.�A.JA,�A+�#A*��A)�wA)dZA)�A'�A%�A$jA#dZA"1'A �A�A1AA�TA��AXA�HA��A�^A�AhsAZA��A�AI�A�A�yA��AVA|�A
�HA
�uA
{A	�TA	G�AbNA`BA33A�A=qA��Ap�A?}A�A��A�;A��AO�AC�A �y@��w@��+@�hs@�I�@�dZ@�=q@���@�|�@�@���@�w@�@�r�@�F@�ȴ@��`@�M�@��`@�9X@�@�v�@�u@� �@��@�l�@��y@���@�O�@��D@߾w@���@��@ޏ\@���@�bN@��@�@ش9@�b@��y@��@�7L@�t�@�9X@·+@�-@͡�@̋D@˝�@ʰ!@�p�@ȃ@Ǖ�@�E�@�hs@ēu@�t�@��@°!@�~�@�@�Ĝ@�9X@��F@��R@�G�@�Ĝ@���@���@��@�"�@�5?@���@�j@�9X@���@�t�@�+@�@�J@���@��@�?}@�&�@���@�t�@��H@���@��@�/@�bN@�t�@��@��H@��@��-@�9X@���@�-@���@��@�/@���@���@��R@��-@��`@��
@�K�@�ȴ@�^5@���@�@��h@��h@��h@�7L@���@�I�@��@��;@��F@���@�|�@�;d@��H@���@�{@��T@���@��^@���@�`B@��@���@�1'@���@���@�C�@��R@�V@��#@���@�`B@�/@��@�%@��/@��D@�I�@�1@���@�l�@�;d@�o@�~�@�-@�@��@��@��@��@��^@��7@�hs@�/@�/@��`@���@�I�@��@���@��@�t�@�K�@�;d@�o@�@���@���@�v�@�{@��T@���@���@�O�@��@�V@��@��u@�r�@�bN@��@��F@��@�|�@�\)@�S�@�"�@�@��@��y@���@��\@�v�@�5?@��@�@���@�x�@�hs@�G�@�&�@��@��@��@���@��/@���@�r�@�Z@�Z@�Q�@���@���@��@�l�@�S�@�@���@��\@�n�@�V@�J@��#@�@��7@��@���@���@��9@��u@�1'@���@�S�@�o@�o@��H@��!@�^5@�5?@�@���@�p�@��@��@���@��@�b@��@�K�@�@�@�ȴ@��R@���@�v�@��@�@���@��@��#@���@�`B@�&�@���@��@���@���@�r�@�A�@�1@l�@�P@;d@~E�@~5?@}��@}O�@|�@|�@|�@|Z@|1@{�F@{��@{��@{t�@{�@{S�@{"�@{33@z��@zM�@y�7@y7L@xĜ@x1'@wl�@w;d@v�@v�R@v��@v$�@u`B@u�@u�@uO�@tI�@t(�@t9X@t(�@s�
@s��@so@rM�@q��@q��@q&�@p�u@pb@o��@o\)@nȴ@n�+@nff@m�@n@m�h@mV@l�@l��@lz�@l(�@kƨ@k@jn�@j^5@j=q@i��@i�@h  @g
=@f��@fV@e�-@e�@ep�@e`B@e/@d�D@c�
@c"�@b��@b=q@a�@a�7@a7L@`�`@`r�@`  @_�w@_l�@^��@^5?@]p�@^��@]�@]��@^ff@^v�@^ff@]��@]p�@]O�@\��@\I�@[�
@["�@Z�H@ZM�@Y�7@Y&�@W��@W��@V��@VV@U�h@T�/@T�@S�
@Sƨ@Sƨ@S�F@S��@S�@S�@SdZ@SC�@R�@R��@R��@RM�@R=q@Q��@Qx�@Q�7@Q&�@P��@P�9@Pr�@P1'@O�P@O;d@O;d@O+@O+@N�y@N��@Nff@N{@N@M@M�h@Mp�@M/@L�@LI�@L1@K�m@Kƨ@Kƨ@Kƨ@Kƨ@K��@K33@Ko@K@J�H@J��@J�@I��@I&�@H��@HQ�@G�;@G�@G�P@GK�@G�@F�@Fȴ@F�+@F$�@E�@E��@E��@E�@EO�@EV@D�/@D�@DI�@D1@C��@C�m@Cƨ@Ct�@B�H@B~�@B^5@BM�@B=q@A��@A�^@@��@@r�@@ �@?�@?�@?�;@?�;@?�@?+@>ff@>$�@=��@=��@=p�@=O�@=�@<�@<j@<I�@<(�@<1@;��@;�
@;ƨ@;��@;�@;o@:��@:��@:�\@:^5@:M�@:J@9��@97L@9%@8��@8Ĝ@8b@7�;@7��@7�@7K�@7
=@6�@6�R@6v�@6E�@6$�@6@5��@5�h@5O�@4��@4�j@4�D@4Z@49X@4�@3ƨ@333@3@2�@2�H@2�!@2�!@2��@2M�@2�@2�@2J@1�@1�#@1�^@1��@1X@17L@17L@17L@1�@0�`@0�9@0�u@0bN@0A�@0 �@/�;@/�w@/��@/|�@/;d@/�@.�y@.ȴ@.��@.V@.5?@-�T@-@-�-@-`B@-?}@-�@-V@,��@,Z@+�
@+ƨ@+�@+33@*��@*^5@)�#@)��@)��@)X@)�@(Ĝ@(�u@(bN@(1'@(  @'�@'|�@'\)@'�@&�y@&�@&ȴ@&��@&��@&�+@&ff@&$�@%�@%`B@%V@$�/@$�@$�/@$�j@$z�@$I�@$(�@#��@#�
@#�F@#�@#t�@#"�@"�!@"�\@"n�@!�#@!�^@!��@!��@!hs@!&�@ ��@ �u@ bN@  �@��@K�@��@ȴ@v�@E�@@�@�@�-@O�@V@�@��@�D@I�@ƨ@�F@t�@"�@@�H@�!@^5@-@�@J@��@�#@�^@��@��@�7@hs@X@7L@&�@�@�9@A�@ �@  @��@�@|�@+@�y@�R@V@{@�@�T@`B@�@��@�j@z�@Z@�@��@�m@�
@ƨ@�F@��@��@�@S�@�@�H@��@��@=q@-@��@��@��@7L@��@�@A�@1'@b@��@��@l�@K�@;d@��@�@ȴ@�R@��@v�@V@@@�@�@�/@�D@(�@�
@ƨ@��@S�@"�@
�@
�!@
M�@
-@	�#@	��@	�7@	x�@	x�@	hs@	hs@	X@	X@	G�@	G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B
%B
PB
�B
!�B
+B
5?B
>wB
G�B
P�B
_;B
n�B
s�B
~�B
�B
�hB
��B
�-B
��B
��B
�HB\BG�Bn�B��B��B��B�wBŢB�LB��B��B  B  B��B�B�B�mB�fB��B��B��B��B��B�mB��B��B��B�B�/B��B@�BF�BO�BdZBk�BdZBbNBS�B/B/B33B49B49B<jBO�B:^B9XB33B+B33B0!B)�B=qB<jB?}BZB[#BI�B�B�B�/BƨB��B�B`BB;dBoB
��B
�B
�/B
�B
��B
ǮB
�XB
��B
}�B
m�B
e`B
A�B
  B	�ZB	�LB	��B	�B	�JB	�VB	�7B	{�B	t�B	cTB	YB	W
B	VB	I�B	F�B	;dB	D�B	L�B	T�B	aHB	gmB	ffB	cTB	]/B	^5B	VB	J�B	B�B	6FB	2-B	-B	$�B	uB	%B��B�B�B�B�`B�HB�/B��BǮB��B��BȴB�RB�B��B��B��B�uB�\B�PB�%B�B{�B|�B|�Bz�Bw�Bw�Bv�Bv�Bq�Bl�BjBhsBbNBdZB`BB]/BXBYBW
BVBT�BVBT�BS�BP�BN�BO�BI�BH�BG�BG�BC�BB�BA�B@�BA�BB�BF�BC�B<jB=qB;dB:^B:^B9XB9XB9XB8RB;dB5?B33B33B33B2-B/B/B,B,B,B0!B49B1'B/B.B/B/B.B.B0!B49B49B33B33B6FB8RB9XB9XB9XB:^B;dB<jB=qB>wB?}B?}B@�B@�BG�BK�BN�BQ�BP�BT�BVBXB[#B`BBaHBaHBbNBe`BffBhsBhsBgmBhsBhsBiyBo�Br�Br�Bt�Bu�Bx�Bx�Bx�B{�B|�Bz�By�B{�B~�B~�B}�B}�B}�B�B�B�+B�7B�DB�DB�bB�oB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B�-B�-B�B�B�B�!B�3B�3B�?B�RB�XB�dB�jBBĜBǮB��B��B��B�
B�)B�BB�NB�sB�B�B�B��B��B��B��B��B��B��B	B	B		7B	VB	oB	{B	�B	�B	 �B	"�B	'�B	+B	/B	2-B	6FB	6FB	7LB	9XB	;dB	>wB	?}B	A�B	B�B	G�B	H�B	I�B	J�B	N�B	Q�B	R�B	S�B	S�B	S�B	S�B	VB	W
B	YB	\)B	]/B	^5B	`BB	bNB	e`B	ffB	gmB	hsB	iyB	iyB	jB	l�B	m�B	n�B	p�B	s�B	t�B	t�B	w�B	z�B	}�B	}�B	~�B	�B	�B	�B	�=B	�JB	�JB	�VB	�\B	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�?B	�RB	�RB	�XB	�XB	�dB	�wB	�}B	�}B	��B	ÖB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�;B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
JB
JB
JB
PB
PB
JB
JB
JB
JB
JB
JB
DB
DB

=B
DB
DB
bB
\B
\B
hB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
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
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
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
:^B
:^B
:^B
:^B
;dB
;dB
;dB
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
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
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
M�B
M�B
M�B
M�B
M�B
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
S�B
S�B
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
XB
XB
XB
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
\)B
\)B
\)B
\)B
]/B
]/B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
gmB
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
jB
jB
jB
jB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�vB	�|B	�B	�|B	�vB	�cB	�WB	�WB	�QB	�WB	�WB	�]B	�cB	�iB	�iB	�vB	�vB	�B	��B	��B	��B
�B
B
MB
!�B
*�B
5B
>BB
GzB
P�B
_B
ncB
s�B
~�B
��B
�4B
�_B
��B
̘B
ҽB
�B(BGzBncB��B��B�~B�BB�mB�BϫB��B��B��B�B�WB�KB�8B�2B��B��B��B��B��B�8B��BΊBΥB��B��B��B@OBFtBO�Bd&BkQBd&Ba�BS�B.�B.�B2�B4B4B<6BO�B:*B9$B2�B*�B2�B/�B)�B=<B<6B?HBY�BZ�BI�B�B�iB��B�YB��B��B`B;0B:B
��B
�oB
��B
��B
ϑB
�zB
�$B
�WB
}�B
m]B
e,B
AUB	��B	�B	��B	�xB	��B	�B	�"B	��B	{�B	t�B	c B	X�B	V�B	U�B	I�B	FtB	;B	DgB	L~B	T�B	`�B	gB	fB	cB	\�B	]�B	U�B	J�B	B[B	5�B	1�B	,�B	$�B	@B	�B��B�B�UB�0B�,B��B��B�xB�_BϑB�~B�fB�B��B��B�QB�?B�&B�B�B��B��B{�B|�B|�Bz�Bw�Bw�Bv�BvzBq[Bl=Bj0Bh$BbBdB`B\�BW�BX�BV�BU�BT�BU�BT�BS�BP�BN�BO�BIlBHfBG_BG_BCGBBABA;B@4BA;BBABFYBCGB<B="B;B:B:B9	B9	B9	B8B;B4�B2�B2�B2�B1�B.�B.�B+�B+�B+�B/�B3�B0�B.�B-�B.�B.�B-�B-�B/�B3�B3�B2�B2�B5�B8B9	B9	B9	B9�B;B<B="B>(B?.B?.B@4B@4BG_BKxBN�BQ�BP�BT�BU�BW�BZ�B_�B`�B`�Ba�BeBfBh$Bh$BgBh$Bh$Bi*BoOBraBrGBtTBuZBx�Bx�Bx�B{�B|�BzxBy�B{B~�B~�B}�B}�B}�B��B��B��B��B��B��B�B� B� B�B�,B�9B�WB�dB�pB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�AB�MB�_B�xBѝBөBּB��B��B��B�$B�=B�/B�aB�TB�zB��B��B��B��B��B	 �B	�B	�B	B	 B	,B	EB	jB	 vB	"hB	'�B	*�B	.�B	1�B	5�B	5�B	6�B	9	B	;B	>B	?.B	A;B	BAB	G_B	HfB	IlB	JrB	N�B	Q�B	R�B	S�B	S�B	S�B	S�B	U�B	V�B	X�B	[�B	\�B	]�B	_�B	a�B	eB	fB	gB	h$B	iB	iB	j0B	l"B	m)B	n/B	pUB	shB	tnB	tnB	w�B	z�B	}�B	}�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�,B	�,B	�2B	�?B	�=B	�]B	�VB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�	B	�B	�(B	�B	�B	�4B	�GB	�?B	�_B	�fB	�KB	�XB	�^B	�~B	̈́B	�vB	ЗB	ѝB	ңB	ңB	ԯB	ԯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	�B	�B	�$B	�$B	�B	�*B	�0B	�6B	�=B	�IB	�OB	�[B	�[B	�hB	�TB	�nB	�hB	�MB	�hB	�hB	�hB	�TB	�tB	�zB	�zB	�fB	�rB	��B	��B	��B	��B	�B	��B	�B	�B	�xB	�xB	�B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
 �B
 �B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
B
�B
�B
B
B
B
�B
�B
�B
B
B
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
B
B
B
 B
&B
,B
,B
B
,B
,B
9B
9B
?B
?B
KB
KB
1B
EB
KB
QB
QB
QB
WB
dB
dB
jB
pB
VB
pB
pB
pB
 \B
 vB
"hB
"�B
#�B
%�B
%zB
%�B
&�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
8B
8B
9	B
9	B
9	B
8�B
8�B
8�B
9	B
:B
:B
9�B
:B
:B
:B
:B
;B
;B
;B
;B
;B
;B
;B
<B
<B
<B
="B
=B
=B
>B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
?.B
?.B
?.B
?.B
@4B
@B
@4B
@4B
A B
A B
A;B
A;B
A B
BAB
BAB
BAB
BAB
BAB
B'B
BAB
CGB
CGB
CGB
C-B
CGB
CGB
C-B
CGB
C-B
D3B
DMB
DMB
DMB
DMB
DMB
D3B
ESB
E9B
ESB
ESB
ESB
ESB
FYB
FYB
FYB
FYB
F?B
G_B
G_B
G_B
G_B
G_B
GEB
HfB
HfB
HfB
HfB
HKB
IRB
IlB
IlB
IRB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
KxB
KxB
L~B
L~B
L~B
L~B
LdB
LdB
L~B
M�B
M�B
M�B
MjB
M�B
M�B
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
O�B
O�B
OvB
OvB
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P}B
P�B
P�B
P�B
P}B
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
U�B
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
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
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
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
b�B
cB
b�B
cB
cB
cB
cB
cB
c�B
dB
c�B
dB
eB
eB
eB
fB
gB
gB
gB
gB
h$B
h$B
h$B
h$B
h$B
i*B
i*B
i*B
i*B
i*B
i*B
jB
j0B
j0B
j0B
j0B
j01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.63(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905130036192019051300361920190513003619201905140022252019051400222520190514002225JA  ARFMdecpA19c                                                                20190508033635  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190507183640  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190507183642  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190507183642  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190507183643  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190507183643  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190507183643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190507183643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190507183644  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190507183644                      G�O�G�O�G�O�                JA  ARUP                                                                        20190507185644                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190508153459  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190512153619  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190512153619  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190513152225  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                