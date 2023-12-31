CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-11-22T11:00:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191122110025  20191122110025  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @������F1   @������@+�     �d�^5?|�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP��BW��B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D	  D	� D
  D
� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@���A
{A*{AJ{Aj{A�
=A�
=A�
=A�
=A��
A�
=A�
=A�
=B�B
�B�B�B"�B*�B2�B:�BB�BJ�BSQ�BZ�Bb�Bj�Br�Bz�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �HC�HC�HC�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD��D!�D�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD!�D��D(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt�RDu(RDu�RDv(RDv�RDw(RDw�RDx(RDx�RDy(RDy�RDz(RDz�RD{(RD{�RD|(RD|�RD}(RD}�RD~(RD~�RD(RD�RD�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��\D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�P�D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D���D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D)D��)D�)D�T)DÔ)D��)D�)D�T)DĔ)D��)D�)D�T)DŔ)D��)D�)D�T)DƔ)D��)D�)D�T)Dǔ)D��)D�)D�T)DȔ)D��)D�)D�T)Dɔ)D��)D�)D�T)Dʔ)D��)D�)D�T)D˔)D��)D�)D�T)D̔)D��)D�)D�T)D͔)D��)D�)D�T)DΔ)D��)D�)D�T)Dϔ)D��)D�)D�T)DД)D��)D�)D�T)Dє)D��)D�)D�T)DҔ)D��)D�)D�T)DӔ)D��)D�)D�T)DԔ)D��)D�)D�T)DՔ)D��)D�)D�T)D֔)D��)D�)D�T)Dה)D��)D�)D�T)Dؔ)D��)D�)D�T)Dٔ)D��)D�)D�T)Dڔ)D��)D�)D�T)D۔)D��)D�)D�T)Dܔ)D��)D�)D�T)Dݔ)D��)D�)D�W\Dޔ)D��)D�)D�T)Dߔ)D��)D�)D�T)D��)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�W\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�bA�bA�oA�{A�{A�{A��A��A��A��A��A��A��A��A� �A� �A� �A�"�A�"�A�"�A�$�A��A�bA�JA�XAܩ�Aڕ�A��
A��`A�z�A�1'A� �Aϰ!A�M�A�l�A��#A���A˰!A�$�A���Aŕ�A�5?A��A�\)A�;dA�S�A�XA��FA��7A�bNA�ȴA���A�x�A�A��RA���A��A�oA��A���A���A��
A��-A���A�7LA�{A���A���A���A���A���A���A���A���A���A�K�A�+A���A�9XA�33A�dZA�S�A��A�  A��A�v�A�G�A��A�"�A�$�A�I�A~�\A{l�AuAsAp��An��Al��Ai+Ad�A_��AYO�AO��AK�FAJ$�AH�yAHE�AG/AE�AC+A@��A?��A?7LA> �A<z�A;33A8ȴA3l�A0��A-S�A)��A)+A(�RA(ZA'�A'K�A&5?A%�A$ZA"E�A bA�A�/A��A�AoA�HA(�AhsA%A��A\)AK�AC�A�A�+A=qA�
AdZA
=A�jAbNA5?Ap�AK�AG�A;dA��AbA�wAƨAƨA�;A�A��A|�A�A��A~�A{A��A/A�Ar�AbA�At�A
��A
JA	?}A�A��A��A�9A^5A��A�AE�A�wA33A��A~�A�A�FA�PA|�A;dA�!A��A�\A�TA��AJAJA�TA ��A ^5@���@��y@�1'@���@���@��@�bN@�I�@��@���@��P@�K�@�+@�
=@��@���@�ff@��@�Z@���@��
@�F@���@�hs@��`@�  @�;d@���@@�-@홚@���@���@�33@�\@�J@�^@�G�@�1'@�|�@�@�!@�~�@�@�j@���@㝲@�S�@��y@�ȴ@���@�R@��@◍@�\@�+@�v�@ᙚ@ߍP@�o@އ+@�n�@�$�@��T@���@ݡ�@�x�@��@܃@�I�@�t�@�5?@���@�dZ@ְ!@�5?@��@Ցh@�O�@�V@���@Ԭ@ԓu@ԃ@�z�@�bN@�Q�@�I�@�1'@�1@ӥ�@��@�^5@�@��T@с@��@Гu@�bN@�9X@ϕ�@���@θR@͙�@�(�@�l�@�33@ʧ�@�v�@��T@���@�(�@ǝ�@���@���@��@��/@Ĭ@ċD@�bN@� �@��;@�;d@�ȴ@�ff@��#@�7L@�r�@��y@��@��u@�r�@�1'@�  @���@�t�@�S�@�o@��@���@���@���@�r�@�bN@�9X@��@���@�\)@�+@�33@��@�ȴ@�V@�=q@�5?@��#@�V@��@���@���@��j@��9@��@�I�@�  @���@�dZ@�
=@���@�v�@��@��7@�O�@��@�%@���@��`@�Ĝ@��u@�z�@�Q�@��;@�@��!@��@�x�@���@���@�\)@��@��@���@���@�M�@���@��h@�hs@�7L@��@���@�(�@�b@�  @��;@��w@���@�K�@���@���@�^5@�{@��@���@�`B@��@��@�I�@� �@�1@�ƨ@�o@�^5@�J@���@��h@�x�@�X@�&�@��@��`@��D@��;@�ƨ@��w@��@���@��P@��@�dZ@�;d@�@���@�ff@�-@���@�p�@�?}@���@� �@��@��\@�x�@���@���@��@�1@���@���@�C�@��y@���@�^5@�5?@��@��@�G�@�%@��@�I�@��F@�33@�o@�ȴ@��+@�n�@�^5@�M�@�{@��T@�@��-@��h@��@�p�@�G�@��@���@���@��D@��@���@��P@�;d@�
=@�~�@�J@��@�`B@�7L@��/@���@��@��@��+@��@��^@��@��j@��9@��9@��@���@�z�@��@��@��@��m@��;@��;@��;@��;@���@���@�\)@�@���@��+@�v�@�E�@�$�@�J@��@��#@���@�p�@�O�@�O�@�%@��@�Z@��@��@�33@�~�@�$�@�J@�@���@��@��@��@��T@��#@�@���@�`B@�&�@��j@�  @~�R@}�h@}O�@|�/@|�D@{"�@z�\@z=q@y�#@y��@y�@x�u@xQ�@x  @w�;@w�w@wl�@w�@v�R@v$�@u�T@u�h@uO�@t�@tj@r�\@q��@q�7@qhs@qX@qX@q&�@p��@p�u@pr�@pQ�@p1'@p �@o�@o�;@o�w@oK�@n��@m��@m/@l��@l��@l�D@lj@lZ@l1@k��@k33@j�@j�!@jn�@j-@i�@i��@i&�@h��@h�u@hQ�@hb@g�w@gK�@fȴ@f�+@fV@fE�@f5?@f5?@e��@ep�@d�@d�/@dz�@c��@c��@c"�@b�!@b=q@a�^@a�7@a�7@ax�@aX@`��@`��@`��@`��@`��@`Ĝ@`�u@`Q�@_;d@^�y@^�R@^��@^��@^v�@^ff@^5?@]�@]?}@]V@\��@\�D@[@Z^5@Z�@Y�#@X�`@XA�@WK�@U�T@U�h@UV@TI�@T�@S��@S�@SdZ@S33@R�H@R��@R�@Q�#@Q��@Qhs@Q&�@Q&�@P��@PĜ@PbN@P �@O�w@O�P@O;d@N�@N�+@N$�@M@Mp�@M?}@M�@MV@L�j@L�D@Lz�@LI�@K�m@K��@K33@K"�@J�!@J=q@I�@Ix�@HA�@H  @H  @G�@G�P@G�@Fȴ@F��@F$�@E�@EO�@E�@D�/@D�@D��@Dj@DI�@D(�@C��@Bn�@AG�@@A�@?\)@>�+@=`B@<��@<�@<Z@<I�@<(�@<(�@<(�@<�@<�@<1@<1@<1@<1@<1@<1@;��@;��@;�m@;ƨ@;ƨ@;ƨ@;�F@;��@;�@;dZ@;C�@;@:�!@:M�@:J@9��@9hs@9G�@9�@8��@8�@8Q�@7�@7\)@6�@6@5��@5O�@4z�@3�@3"�@3o@3o@3@3@2�@2�H@2�H@2�H@2��@2��@2��@2��@2��@2�!@2��@2~�@2=q@2-@1��@1x�@1x�@1�@0�9@0�9@0�u@0 �@0b@0  @/�@/\)@/K�@/\)@.��@.�+@-p�@,�/@,�/@,��@,��@,�j@,�j@,�@,��@,Z@,9X@+��@+�m@+ƨ@+dZ@+33@+o@*�@*��@*�\@)��@'�@'�@'l�@'�@&��@&ȴ@&ff@%�T@%p�@$�/@$9X@#��@#�@#S�@#@"��@"~�@!�^@!x�@!x�@!x�@!�7@!�7@!�7@!X@!7L@!%@�;@�@�@ȴ@��@E�@$�@{@�T@@�-@��@p�@�@��@z�@9X@(�@(�@(�@(�@��@S�@C�@C�@33@33@33@"�@o@@��@-@��@��@��@��@�^@�^@�^@��@x�@G�@�`@�`@Ĝ@�9@�9@�9@�9@�9@�9@�9@��@�u@�u@�u@�@�@�@bN@A�@ �@b@�w@l�@�@�y@��@V@@��@��@Z@9X@(�@�m@t�@�@��@�#@7L@%@�9@��@bN@b@��@\)@
=@�@�@�@�@�@ȴ@�R@�+@�@/@V@�@z�@��@��@�m@�
@ƨ@�F@��@��@��@�@t�@t�@S�@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�bA�bA�oA�{A�{A�{A��A��A��A��A��A��A��A��A� �A� �A� �A�"�A�"�A�"�A�$�A��A�bA�JA�XAܩ�Aڕ�A��
A��`A�z�A�1'A� �Aϰ!A�M�A�l�A��#A���A˰!A�$�A���Aŕ�A�5?A��A�\)A�;dA�S�A�XA��FA��7A�bNA�ȴA���A�x�A�A��RA���A��A�oA��A���A���A��
A��-A���A�7LA�{A���A���A���A���A���A���A���A���A���A�K�A�+A���A�9XA�33A�dZA�S�A��A�  A��A�v�A�G�A��A�"�A�$�A�I�A~�\A{l�AuAsAp��An��Al��Ai+Ad�A_��AYO�AO��AK�FAJ$�AH�yAHE�AG/AE�AC+A@��A?��A?7LA> �A<z�A;33A8ȴA3l�A0��A-S�A)��A)+A(�RA(ZA'�A'K�A&5?A%�A$ZA"E�A bA�A�/A��A�AoA�HA(�AhsA%A��A\)AK�AC�A�A�+A=qA�
AdZA
=A�jAbNA5?Ap�AK�AG�A;dA��AbA�wAƨAƨA�;A�A��A|�A�A��A~�A{A��A/A�Ar�AbA�At�A
��A
JA	?}A�A��A��A�9A^5A��A�AE�A�wA33A��A~�A�A�FA�PA|�A;dA�!A��A�\A�TA��AJAJA�TA ��A ^5@���@��y@�1'@���@���@��@�bN@�I�@��@���@��P@�K�@�+@�
=@��@���@�ff@��@�Z@���@��
@�F@���@�hs@��`@�  @�;d@���@@�-@홚@���@���@�33@�\@�J@�^@�G�@�1'@�|�@�@�!@�~�@�@�j@���@㝲@�S�@��y@�ȴ@���@�R@��@◍@�\@�+@�v�@ᙚ@ߍP@�o@އ+@�n�@�$�@��T@���@ݡ�@�x�@��@܃@�I�@�t�@�5?@���@�dZ@ְ!@�5?@��@Ցh@�O�@�V@���@Ԭ@ԓu@ԃ@�z�@�bN@�Q�@�I�@�1'@�1@ӥ�@��@�^5@�@��T@с@��@Гu@�bN@�9X@ϕ�@���@θR@͙�@�(�@�l�@�33@ʧ�@�v�@��T@���@�(�@ǝ�@���@���@��@��/@Ĭ@ċD@�bN@� �@��;@�;d@�ȴ@�ff@��#@�7L@�r�@��y@��@��u@�r�@�1'@�  @���@�t�@�S�@�o@��@���@���@���@�r�@�bN@�9X@��@���@�\)@�+@�33@��@�ȴ@�V@�=q@�5?@��#@�V@��@���@���@��j@��9@��@�I�@�  @���@�dZ@�
=@���@�v�@��@��7@�O�@��@�%@���@��`@�Ĝ@��u@�z�@�Q�@��;@�@��!@��@�x�@���@���@�\)@��@��@���@���@�M�@���@��h@�hs@�7L@��@���@�(�@�b@�  @��;@��w@���@�K�@���@���@�^5@�{@��@���@�`B@��@��@�I�@� �@�1@�ƨ@�o@�^5@�J@���@��h@�x�@�X@�&�@��@��`@��D@��;@�ƨ@��w@��@���@��P@��@�dZ@�;d@�@���@�ff@�-@���@�p�@�?}@���@� �@��@��\@�x�@���@���@��@�1@���@���@�C�@��y@���@�^5@�5?@��@��@�G�@�%@��@�I�@��F@�33@�o@�ȴ@��+@�n�@�^5@�M�@�{@��T@�@��-@��h@��@�p�@�G�@��@���@���@��D@��@���@��P@�;d@�
=@�~�@�J@��@�`B@�7L@��/@���@��@��@��+@��@��^@��@��j@��9@��9@��@���@�z�@��@��@��@��m@��;@��;@��;@��;@���@���@�\)@�@���@��+@�v�@�E�@�$�@�J@��@��#@���@�p�@�O�@�O�@�%@��@�Z@��@��@�33@�~�@�$�@�J@�@���@��@��@��@��T@��#@�@���@�`B@�&�@��j@�  @~�R@}�h@}O�@|�/@|�D@{"�@z�\@z=q@y�#@y��@y�@x�u@xQ�@x  @w�;@w�w@wl�@w�@v�R@v$�@u�T@u�h@uO�@t�@tj@r�\@q��@q�7@qhs@qX@qX@q&�@p��@p�u@pr�@pQ�@p1'@p �@o�@o�;@o�w@oK�@n��@m��@m/@l��@l��@l�D@lj@lZ@l1@k��@k33@j�@j�!@jn�@j-@i�@i��@i&�@h��@h�u@hQ�@hb@g�w@gK�@fȴ@f�+@fV@fE�@f5?@f5?@e��@ep�@d�@d�/@dz�@c��@c��@c"�@b�!@b=q@a�^@a�7@a�7@ax�@aX@`��@`��@`��@`��@`��@`Ĝ@`�u@`Q�@_;d@^�y@^�R@^��@^��@^v�@^ff@^5?@]�@]?}@]V@\��@\�D@[@Z^5@Z�@Y�#@X�`@XA�@WK�@U�T@U�h@UV@TI�@T�@S��@S�@SdZ@S33@R�H@R��@R�@Q�#@Q��@Qhs@Q&�@Q&�@P��@PĜ@PbN@P �@O�w@O�P@O;d@N�@N�+@N$�@M@Mp�@M?}@M�@MV@L�j@L�D@Lz�@LI�@K�m@K��@K33@K"�@J�!@J=q@I�@Ix�@HA�@H  @H  @G�@G�P@G�@Fȴ@F��@F$�@E�@EO�@E�@D�/@D�@D��@Dj@DI�@D(�@C��@Bn�@AG�@@A�@?\)@>�+@=`B@<��@<�@<Z@<I�@<(�@<(�@<(�@<�@<�@<1@<1@<1@<1@<1@<1@;��@;��@;�m@;ƨ@;ƨ@;ƨ@;�F@;��@;�@;dZ@;C�@;@:�!@:M�@:J@9��@9hs@9G�@9�@8��@8�@8Q�@7�@7\)@6�@6@5��@5O�@4z�@3�@3"�@3o@3o@3@3@2�@2�H@2�H@2�H@2��@2��@2��@2��@2��@2�!@2��@2~�@2=q@2-@1��@1x�@1x�@1�@0�9@0�9@0�u@0 �@0b@0  @/�@/\)@/K�@/\)@.��@.�+@-p�@,�/@,�/@,��@,��@,�j@,�j@,�@,��@,Z@,9X@+��@+�m@+ƨ@+dZ@+33@+o@*�@*��@*�\@)��@'�@'�@'l�@'�@&��@&ȴ@&ff@%�T@%p�@$�/@$9X@#��@#�@#S�@#@"��@"~�@!�^@!x�@!x�@!x�@!�7@!�7@!�7@!X@!7L@!%@�;@�@�@ȴ@��@E�@$�@{@�T@@�-@��@p�@�@��@z�@9X@(�@(�@(�@(�@��@S�@C�@C�@33@33@33@"�@o@@��@-@��@��@��@��@�^@�^@�^@��@x�@G�@�`@�`@Ĝ@�9@�9@�9@�9@�9@�9@�9@��@�u@�u@�u@�@�@�@bN@A�@ �@b@�w@l�@�@�y@��@V@@��@��@Z@9X@(�@�m@t�@�@��@�#@7L@%@�9@��@bN@b@��@\)@
=@�@�@�@�@�@ȴ@�R@�+@�@/@V@�@z�@��@��@�m@�
@ƨ@�F@��@��@��@�@t�@t�@S�@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	-B	-B	-B	-B	-B	-B	-B	-B	-B	-B	-B	-B	-B	-B	.B	.B	.B	.B	.B	.B	.B	.B	/B	2-B	33B	K�B	ƨB	��B	��B
�B
'�B
#�B
�B
!�B
�1B$�B|�B�FBB��BW
B=qB
=B
��B
��B
�HB
�
B
��B�B,B=qBK�Bk�B�7B��B��B��B�FB��B��B��B�XB�jB�qB�}B��BǮB��B�{B�PB�Bv�BgmB`BB]/BT�BH�B=qB8RB1'B%�B�BPB
��B
�B
��B
��B
�B
�=B
cTB
G�B
0!B
�B
JB	�B	�)B	��B	ǮB	�dB	�B	��B	�=B	t�B	ZB	R�B	O�B	L�B	I�B	F�B	C�B	A�B	B�B	=qB	:^B	33B	%�B	�B	�B	{B	{B	�B	�B	�B	�B	�B	 �B	#�B	'�B	(�B	49B	=qB	G�B	Q�B	YB	^5B	e`B	iyB	p�B	� B	�=B	�=B	�{B	��B	��B	��B	��B	��B	��B	�{B	�{B	�uB	�oB	�\B	�PB	�VB	��B	��B	��B	��B	��B	�!B	�3B	�9B	�FB	ÖB	��B	��B	��B	��B	��B	��B	��B	�B	�ZB	�fB	�fB	�`B	�fB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
hB
oB
oB
uB
uB
oB
VB
PB
VB
oB
uB
uB
uB
uB
uB
uB
oB
oB
oB
oB
oB
oB
{B
{B
�B
�B
�B
{B
�B
�B
�B
{B
uB
uB
hB
\B
PB
JB
JB
JB
VB
VB
VB
VB
PB
PB
PB
JB
PB
\B
hB
oB
uB
{B
{B
{B
{B
{B
uB
uB
uB
oB
oB
hB
hB
hB
bB
hB
bB
bB
bB
bB
\B
\B
VB
PB
PB
JB
JB
JB
DB
DB

=B

=B

=B

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
VB
\B
\B
VB
\B
\B
VB
VB
bB
bB
bB
bB
bB
oB
hB
hB
uB
{B
uB
oB
oB
oB
oB
hB
hB
hB
oB
oB
oB
oB
oB
hB
oB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
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
(�B
)�B
)�B
,B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
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
33B
33B
33B
49B
49B
5?B
5?B
6FB
5?B
6FB
5?B
5?B
5?B
5?B
5?B
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
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
A�B
B�B
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
E�B
E�B
F�B
G�B
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
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
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
XB
XB
XB
XB
XB
XB
XB
W
B
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
[#B
[#B
\)B
\)B
\)B
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
_;B
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
jB
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
� B
� B
� B
� B
~�B
~�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�+B
�1B
�1B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�1B
�7B
�7B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
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
�oB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	;|B	�]B	��B	�B
BB
�B
�B
gB
�B
w�B�Bl�B��B�DB�sBF�B-&B
��B
��B
�qB
��B
ƿB
�B
UB�B-&B;|B[:Bx�B�UB��B��B��B��B��B��B�B�B�&B�2B��B�cB��B�0B}Br�Bf~BW"BO�BL�BD�B8iB-&B(B �B�B
UB
�B
�B
��B
�|B
�8B
��B
y�B
S	B
7cB
�B
sB	��B	�4B	��B	��B	�cB	�B	��B	�OB	y�B	dqB	I�B	B�B	?�B	<�B	9oB	6]B	3KB	1>B	2DB	-&B	*B	"�B	�B	gB	HB	0B	0B	HB	aB	[B	aB	gB	zB	�B	�B	�B	#�B	-&B	7cB	A�B	H�B	M�B	UB	Y.B	`YB	o�B	y�B	y�B	�0B	�BB	�BB	�<B	�BB	�BB	�6B	�0B	�0B	�*B	�$B	B	}B	~B	�6B	�OB	��B	��B	��B	��B	��B	��B	��B	�KB	��B	íB	ĳB	§B	§B	��B	§B	��B	�B	�B	�B	�B	�B	�B	�.B	�SB	�_B	�kB	�kB	�kB	�kB	�_B	�kB	�~B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B
B
$B
$B
*B
*B
$B	�B	�B	�B
$B
*B
*B
*B
*B
*B
*B
$B
$B
$B
$B
$B
$B
0B
0B
6B
<B
<B
0B
6B
6B
6B
0B
*B
*B
B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B
B
$B
*B
0B
0B
0B
0B
0B
*B
*B
*B
$B
$B
B
B
B
 B
B
 B
 B
 B
 B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
 B
 B
 B
 B
$B
B
B
*B
0B
*B
$B
$B
$B
$B
B
B
B
$B
$B
$B
$B
$B
B
$B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
6B
6B
6B
6B
6B
6B
6B
6B
6B
6B
6B
6B
6B
0B
0B
0B
*B
*B
*B
0B
6B
BB
	OB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB
[B
[B
[B
[B

UB

UB

UB

UB

UB
[B
[B
aB
gB
gB
sB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
!�B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
%�B
$�B
$�B
$�B
$�B
$�B
'B
(B
(B
(B
)B
*B
*B
*B
*B
*B
*B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-&B
-&B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
.,B
/2B
/2B
/2B
/2B
/2B
1>B
2DB
3KB
3KB
3KB
3KB
3KB
3KB
4QB
4QB
4QB
4QB
5WB
5WB
6]B
7cB
8iB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
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
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
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
G�B
F�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
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
P�B
P�B
P�B
P�B
RB
RB
RB
RB
RB
S	B
S	B
S	B
TB
TB
TB
TB
TB
UB
UB
UB
UB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W"B
X(B
Y.B
Y.B
Z4B
[:B
[:B
[:B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
`YB
`YB
`YB
a_B
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
dqB
dqB
dqB
dqB
exB
exB
dqB
exB
exB
f~B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
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
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
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
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
v�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
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
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
}B
}B
}B
}B
}B
}B
}B
}B
}B
~B
~B
B
B
B
B
B
B
B
B
B
�B
�B
�B
�B
�$B
�*B
�*B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.63 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20191122110025              20191122110025  AO  ARCAADJP                                                                    20191122110025    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20191122110025    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191122110025  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191122110025  QCF$                G�O�G�O�G�O�0               