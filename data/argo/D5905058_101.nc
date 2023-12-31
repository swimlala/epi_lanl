CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-05T03:35:41Z creation;2018-11-05T03:35:44Z conversion to V3.1;2019-12-23T06:12:32Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181105033541  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA  I2_0675_101                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؎A0���1   @؎A�>� @6��t��cT�O�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @[�@�(�@�(�A
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
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt�RDu(RDu�RDv(RDv�RDw(RDw�RDx(RDx�RDy(RDy�RDz(RDz�RD{(RD{�RD|(RD|�RD}(RD}�RD~(RD~�RD(RD�RD�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D���D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D)D��)D�)D�T)DÔ)D��)D�)D�T)DĔ)D��)D�)D�T)DŔ)D��)D�)D�T)DƔ)D��)D��D�T)Dǔ)D��)D�)D�T)DȔ)D��)D�)D�T)Dɔ)D��)D�)D�T)Dʔ)D��)D�)D�T)D˔)D��)D�)D�T)D̔)D��)D�)D�T)D͔)D��)D�)D�T)DΔ)D��)D�)D�T)Dϔ)D��)D�)D�T)DД)D��)D�)D�T)Dє)D��)D�)D�T)DҔ)D��)D�)D�T)DӔ)D��)D�)D�T)DԔ)D��)D�)D�T)DՔ)D��)D�)D�T)D֔)D��)D�)D�T)Dה)D��)D�)D�T)Dؔ)D��)D�)D�T)Dٔ)D��)D�)D�T)Dڔ)D��)D�)D�T)D۔)D��)D�\D�T)Dܔ)D��)D�)D�T)Dݔ)D��)D�)D�T)Dޔ)D��)D�)D�T)Dߔ)D��)D�)D�T)D��)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��\D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�\D��)D�)D�T)D��)D��)D�)D�T)D��\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AУ�A�~�A�|�AЉ7A�t�A�\)A�E�A�;dA�(�A��A�
=A�A���A���A���A��A��A��mA��HA��;A��#A���A�A϶FAϝ�A�l�A�1'A�&�A�bA��A΅AͮAʙ�A�1'A�=qA��^A���A���A���A��A��A���A��+A�ZA���A�A�A�1A�1'A��FA��A�(�A���A�;dA�S�A�S�A��A�bA��A�n�A��A�ƨA�O�A���A�?}A��A��A�^5A��`A��A�XA�XA��9A�S�A���A��A��9A��FA�\)A���A�I�A��A�
=A�r�A� �A��
A�Q�A�C�A�+A�"�A�=qA��yA��\A�Q�A��RA�hsA�M�A�-A�M�A�G�A��7A��A��+A���A�+A��A~=qAz��Aw�^At�9As�hAsO�ArȴAo%Aj$�Ag��Ac��A_XA]"�A[��AZ��AZE�AY��AYoAWƨAUhsAT�!ATr�AS�mAR�yAQ%APbAO�7AN�AL��AJ9XAH�RAGAF�uAD��ACƨAA`BA=�A;�
A9�wA8�/A7ƨA7�A6�A5��A2��A1�A/A-\)A,��A+�A*��A*M�A(n�A&�`A&jA&ffA%��A$�A#?}A"{A!��A!G�A ��A $�A�A/A��AZAƨA=qAXA��A
=Az�A��A�HA  A�FAhsAA+AffA&�AI�A��A�9AA�A
�A	;dA�+Av�A �A��AC�A�HAM�A�;A%A�PA�DAp�A 5?@���@�{@�&�@��@��R@�5?@��@�Q�@�K�@�V@��@�"�@�$�@���@�|�@�M�@���@�@�D@�I�@�1@畁@��@�v�@�7@�1@�ȴ@�E�@��#@�b@�G�@�Q�@�33@���@�M�@�/@ؓu@�r�@�Q�@�9X@�1@׍P@ָR@��@�X@ԃ@�r�@� �@���@�$�@��#@�O�@��m@�dZ@��H@�@��@���@�-@�J@��@��@��#@�I�@�=q@�@�G�@ě�@�r�@�1@�|�@��@��H@�^5@��T@�Q�@��@�@�@�I�@�|�@���@�7L@�Z@���@�+@�?}@���@���@���@�|�@�C�@�v�@�X@���@��9@�ƨ@�
=@�v�@��7@��@�bN@���@�o@�M�@�@���@�x�@�`B@��@�bN@��P@�C�@���@��+@�$�@���@�/@���@��D@�r�@�j@�I�@��;@�dZ@�ȴ@�v�@�n�@�=q@�$�@�{@�{@�{@��@��@�`B@�?}@��@��/@�Ĝ@��D@�|�@��@���@��\@�M�@�J@��@���@��h@��7@�`B@��j@���@���@�|�@�K�@�33@�+@�@��@��\@�v�@�-@��^@�O�@��9@��@�r�@��@�C�@�+@�t�@�o@�v�@�E�@�@��^@���@�x�@�p�@��@�A�@��@�\)@�t�@�|�@�V@���@�5?@��T@�O�@��/@�Ĝ@��j@���@��@�j@��@��@��@��;@���@���@��;@�  @�Z@��@�V@��@�Ĝ@�j@�(�@�b@���@�|�@�S�@�33@��@��@���@�M�@�J@��@��-@��h@�G�@��D@�  @��;@�\)@��@��H@���@��R@�~�@�-@�J@�@���@�`B@�&�@��@��/@��u@� �@��m@��m@��F@��P@�l�@�K�@�"�@��@���@���@�v�@��@���@��@�p�@�hs@�?}@�&�@��@�1@��w@�dZ@�o@�@��@�n�@�=q@��T@��7@�`B@�7L@�%@��j@���@��u@��D@�Q�@�(�@�1@�@
=@~��@~@}O�@}O�@|��@|��@|�@|(�@{ƨ@{t�@{@z�\@y��@y&�@y%@x��@xr�@xA�@x1'@xb@w��@w�P@w+@w
=@v��@vV@v{@u�@t�D@t(�@s��@s�
@s�@s"�@rn�@rJ@q�#@qX@pĜ@pĜ@p��@p�u@pr�@p1'@o�w@ol�@o+@n��@m��@mp�@l��@lZ@l1@k��@kdZ@k33@k"�@j�H@j��@j^5@j�@i��@iG�@h�9@hbN@hb@g��@gl�@g
=@f��@fff@e�@ep�@e/@d��@d��@dz�@dj@dI�@d(�@c��@c�
@cƨ@c��@c33@b��@b��@b��@b�\@bJ@a�@a��@a&�@`��@`�`@`��@`bN@`  @`  @`  @`  @_�@_�@_�w@_;d@^�R@^V@^{@]�@]�-@\��@[��@[t�@[t�@[dZ@[S�@[S�@[S�@[C�@Z��@ZJ@Y��@Yx�@Y7L@Y%@X�`@X�9@X�@W��@W��@Wl�@W\)@W+@V�R@U@Up�@U/@T�@Tz�@T�@Sƨ@S��@S��@SdZ@R�@R�!@R=q@RJ@Q7L@P�9@PbN@O�@O\)@O+@N��@N�R@N�+@NE�@M��@MO�@MV@L�j@LI�@L1@K�F@K�@KC�@J��@J~�@I��@I��@IX@I7L@I%@H��@HQ�@H  @G�@Gl�@G;d@G
=@F�@Fv�@E@E`B@EO�@EV@D��@D��@D�/@D��@D9X@D�@Cƨ@CS�@C33@C@BM�@BJ@A�@A�^@A7L@@Ĝ@@A�@@  @?�@?l�@?K�@?�@>�@>��@>v�@>5?@=��@=�@=/@<��@<��@<�D@<j@<(�@;�m@;�F@;C�@;"�@;@:��@:��@:~�@:=q@:�@9��@9�#@9�7@9G�@97L@9&�@8��@8�u@8�@8r�@8r�@8r�@8r�@8bN@7�@7+@7
=@6�@6�R@6�+@6v�@6E�@6$�@6@5��@5�h@5`B@4��@4�D@4Z@4(�@41@3�
@3�
@3�F@3�@3C�@2�!@2-@1��@1��@1��@1��@1�7@1�@0�`@0Ĝ@0�@01'@/|�@/�@.��@.ff@.@-�@-��@-/@,�/@,�D@,(�@+�F@+��@+dZ@+33@+@+@*��@*��@*~�@*=q@*�@)��@)�@)�#@)�^@)�7@)�@(��@(r�@(  @'��@'�w@'�@'��@'�P@'l�@'\)@'K�@';d@&��@&ȴ@&��@&E�@&{@%�@%��@%/@%V@$�/@$j@$9X@#�F@#S�@#C�@#33@#33@#"�@"�H@"~�@"^5@"�@!�^@!hs@!�@ Ĝ@ ��@ �u@ �@ bN@  �@�w@|�@l�@K�@;d@
=@��@�y@ȴ@��@v�@V@5?@�@�@O�@V@�j@�D@z�@j@Z@I�@(�@(�@(�@�@�m@ƨ@��@dZ@33@�H@��@��@��@��@�\@n�@-@J@�#@��@��@x�@hs@&�@%@�`@�9@r�@ �@�@�;@��@;d@
=@�y@��@��@�+@E�@@�h@?}@V@�@j@9X@1@�
@�F@�@t�@C�@@�H@��@�\@=q@J@�@��@��@x�@X@&�@��@��@�@�@r�@A�@b@  @�@|�@\)@K�@;d@;d@;d@+@+@�y@��@v�@v�@ff@V@{@�T@��@�@�@�@�j@z�@I�@1@��@�m@�
@�F@t�@S�@33@o@
��@
��@
�!@
��@
~�@
^5@
�@	�^@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AУ�A�~�A�|�AЉ7A�t�A�\)A�E�A�;dA�(�A��A�
=A�A���A���A���A��A��A��mA��HA��;A��#A���A�A϶FAϝ�A�l�A�1'A�&�A�bA��A΅AͮAʙ�A�1'A�=qA��^A���A���A���A��A��A���A��+A�ZA���A�A�A�1A�1'A��FA��A�(�A���A�;dA�S�A�S�A��A�bA��A�n�A��A�ƨA�O�A���A�?}A��A��A�^5A��`A��A�XA�XA��9A�S�A���A��A��9A��FA�\)A���A�I�A��A�
=A�r�A� �A��
A�Q�A�C�A�+A�"�A�=qA��yA��\A�Q�A��RA�hsA�M�A�-A�M�A�G�A��7A��A��+A���A�+A��A~=qAz��Aw�^At�9As�hAsO�ArȴAo%Aj$�Ag��Ac��A_XA]"�A[��AZ��AZE�AY��AYoAWƨAUhsAT�!ATr�AS�mAR�yAQ%APbAO�7AN�AL��AJ9XAH�RAGAF�uAD��ACƨAA`BA=�A;�
A9�wA8�/A7ƨA7�A6�A5��A2��A1�A/A-\)A,��A+�A*��A*M�A(n�A&�`A&jA&ffA%��A$�A#?}A"{A!��A!G�A ��A $�A�A/A��AZAƨA=qAXA��A
=Az�A��A�HA  A�FAhsAA+AffA&�AI�A��A�9AA�A
�A	;dA�+Av�A �A��AC�A�HAM�A�;A%A�PA�DAp�A 5?@���@�{@�&�@��@��R@�5?@��@�Q�@�K�@�V@��@�"�@�$�@���@�|�@�M�@���@�@�D@�I�@�1@畁@��@�v�@�7@�1@�ȴ@�E�@��#@�b@�G�@�Q�@�33@���@�M�@�/@ؓu@�r�@�Q�@�9X@�1@׍P@ָR@��@�X@ԃ@�r�@� �@���@�$�@��#@�O�@��m@�dZ@��H@�@��@���@�-@�J@��@��@��#@�I�@�=q@�@�G�@ě�@�r�@�1@�|�@��@��H@�^5@��T@�Q�@��@�@�@�I�@�|�@���@�7L@�Z@���@�+@�?}@���@���@���@�|�@�C�@�v�@�X@���@��9@�ƨ@�
=@�v�@��7@��@�bN@���@�o@�M�@�@���@�x�@�`B@��@�bN@��P@�C�@���@��+@�$�@���@�/@���@��D@�r�@�j@�I�@��;@�dZ@�ȴ@�v�@�n�@�=q@�$�@�{@�{@�{@��@��@�`B@�?}@��@��/@�Ĝ@��D@�|�@��@���@��\@�M�@�J@��@���@��h@��7@�`B@��j@���@���@�|�@�K�@�33@�+@�@��@��\@�v�@�-@��^@�O�@��9@��@�r�@��@�C�@�+@�t�@�o@�v�@�E�@�@��^@���@�x�@�p�@��@�A�@��@�\)@�t�@�|�@�V@���@�5?@��T@�O�@��/@�Ĝ@��j@���@��@�j@��@��@��@��;@���@���@��;@�  @�Z@��@�V@��@�Ĝ@�j@�(�@�b@���@�|�@�S�@�33@��@��@���@�M�@�J@��@��-@��h@�G�@��D@�  @��;@�\)@��@��H@���@��R@�~�@�-@�J@�@���@�`B@�&�@��@��/@��u@� �@��m@��m@��F@��P@�l�@�K�@�"�@��@���@���@�v�@��@���@��@�p�@�hs@�?}@�&�@��@�1@��w@�dZ@�o@�@��@�n�@�=q@��T@��7@�`B@�7L@�%@��j@���@��u@��D@�Q�@�(�@�1@�@
=@~��@~@}O�@}O�@|��@|��@|�@|(�@{ƨ@{t�@{@z�\@y��@y&�@y%@x��@xr�@xA�@x1'@xb@w��@w�P@w+@w
=@v��@vV@v{@u�@t�D@t(�@s��@s�
@s�@s"�@rn�@rJ@q�#@qX@pĜ@pĜ@p��@p�u@pr�@p1'@o�w@ol�@o+@n��@m��@mp�@l��@lZ@l1@k��@kdZ@k33@k"�@j�H@j��@j^5@j�@i��@iG�@h�9@hbN@hb@g��@gl�@g
=@f��@fff@e�@ep�@e/@d��@d��@dz�@dj@dI�@d(�@c��@c�
@cƨ@c��@c33@b��@b��@b��@b�\@bJ@a�@a��@a&�@`��@`�`@`��@`bN@`  @`  @`  @`  @_�@_�@_�w@_;d@^�R@^V@^{@]�@]�-@\��@[��@[t�@[t�@[dZ@[S�@[S�@[S�@[C�@Z��@ZJ@Y��@Yx�@Y7L@Y%@X�`@X�9@X�@W��@W��@Wl�@W\)@W+@V�R@U@Up�@U/@T�@Tz�@T�@Sƨ@S��@S��@SdZ@R�@R�!@R=q@RJ@Q7L@P�9@PbN@O�@O\)@O+@N��@N�R@N�+@NE�@M��@MO�@MV@L�j@LI�@L1@K�F@K�@KC�@J��@J~�@I��@I��@IX@I7L@I%@H��@HQ�@H  @G�@Gl�@G;d@G
=@F�@Fv�@E@E`B@EO�@EV@D��@D��@D�/@D��@D9X@D�@Cƨ@CS�@C33@C@BM�@BJ@A�@A�^@A7L@@Ĝ@@A�@@  @?�@?l�@?K�@?�@>�@>��@>v�@>5?@=��@=�@=/@<��@<��@<�D@<j@<(�@;�m@;�F@;C�@;"�@;@:��@:��@:~�@:=q@:�@9��@9�#@9�7@9G�@97L@9&�@8��@8�u@8�@8r�@8r�@8r�@8r�@8bN@7�@7+@7
=@6�@6�R@6�+@6v�@6E�@6$�@6@5��@5�h@5`B@4��@4�D@4Z@4(�@41@3�
@3�
@3�F@3�@3C�@2�!@2-@1��@1��@1��@1��@1�7@1�@0�`@0Ĝ@0�@01'@/|�@/�@.��@.ff@.@-�@-��@-/@,�/@,�D@,(�@+�F@+��@+dZ@+33@+@+@*��@*��@*~�@*=q@*�@)��@)�@)�#@)�^@)�7@)�@(��@(r�@(  @'��@'�w@'�@'��@'�P@'l�@'\)@'K�@';d@&��@&ȴ@&��@&E�@&{@%�@%��@%/@%V@$�/@$j@$9X@#�F@#S�@#C�@#33@#33@#"�@"�H@"~�@"^5@"�@!�^@!hs@!�@ Ĝ@ ��@ �u@ �@ bN@  �@�w@|�@l�@K�@;d@
=@��@�y@ȴ@��@v�@V@5?@�@�@O�@V@�j@�D@z�@j@Z@I�@(�@(�@(�@�@�m@ƨ@��@dZ@33@�H@��@��@��@��@�\@n�@-@J@�#@��@��@x�@hs@&�@%@�`@�9@r�@ �@�@�;@��@;d@
=@�y@��@��@�+@E�@@�h@?}@V@�@j@9X@1@�
@�F@�@t�@C�@@�H@��@�\@=q@J@�@��@��@x�@X@&�@��@��@�@�@r�@A�@b@  @�@|�@\)@K�@;d@;d@;d@+@+@�y@��@v�@v�@ff@V@{@�T@��@�@�@�@�j@z�@I�@1@��@�m@�
@�F@t�@S�@33@o@
��@
��@
�!@
��@
~�@
^5@
�@	�^@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B`BBbNBbNBbNBbNBbNBbNBaHBbNBaHBaHB`BB`BB`BB`BBaHBaHBaHB`BB`BB_;B_;B]/B\)B_;BaHBcTBffBiyBm�Bm�Bp�B�B�oB��B��B�'B�?B�-B�9B�B��B��B��B��B��B��B��B�hB�VB�7B�%B�B�%B~�B~�B� B�B�7B�bB�\B�=B�=B{�BhsBcTBbNB_;BVBL�B9XB33B1'B �B1B�B�`B��BŢB��B{�Bn�BdZB_;BXBC�B33B;dB;dB,B!�B�B�B�B�B�B�BhB
�B
��B
��B
�-B
��B
��B
�B
l�B
S�B
A�B
)�B
�B
�B
�B	��B	��B	�qB	��B	�B	l�B	cTB	ZB	VB	P�B	L�B	F�B	9XB	33B	1'B	.B	(�B	�B	�B	{B	VB	B��B��B�B�B�/B��BƨB�^B�B��B��B��B��B��B�bB~�Br�Bl�BffBe`BffBdZBbNBe`BaHBffBiyBl�BaHB]/BYBXBZBZBZB^5B]/B^5B_;B_;BZBT�BS�BK�BJ�BI�BH�BF�BE�BB�BB�B>wB<jB:^B8RB7LB6FB5?B33B33B49B0!B0!B0!B0!B/B.B-B,B,B)�B'�B'�B&�B'�B&�B&�B%�B%�B#�B$�B#�B"�B#�B"�B%�B%�B$�B&�B%�B%�B&�B'�B'�B'�B'�B)�B(�B)�B,B,B,B,B,B+B(�B)�B(�B)�B+B,B+B+B+B+B,B.B/B/B/B0!B1'B49B33B2-B1'B49B5?B8RB;dB>wB?}B?}B?}B?}B?}B>wBB�BD�BD�BE�BG�BG�BH�BJ�BK�BK�BL�BM�BQ�BT�BW
BYB]/B_;BcTBhsBjBn�Bn�Bq�Bq�Bv�Bv�Bv�Bw�B{�B� B�B�B�B�B�%B�=B�JB�\B�oB��B��B��B��B��B��B��B��B�B�B�B�-B�FB�XB�wBBĜBŢBŢBƨB��B��B�B�5B�ZB�B�B�B��B��B��B	B	+B	
=B	VB	VB	\B	bB	\B	hB	�B	$�B	&�B	+B	.B	1'B	49B	49B	6FB	9XB	=qB	?}B	@�B	A�B	A�B	A�B	B�B	D�B	G�B	H�B	J�B	K�B	M�B	N�B	P�B	P�B	S�B	W
B	XB	]/B	aHB	gmB	hsB	k�B	k�B	l�B	m�B	n�B	p�B	r�B	q�B	r�B	t�B	w�B	x�B	z�B	�B	�B	�B	�B	�B	�=B	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�?B	�LB	�LB	�XB	�^B	�^B	�XB	�^B	�dB	�dB	�jB	�qB	�}B	��B	B	ÖB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�TB	�ZB	�`B	�mB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
+B
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
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
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
5?B
5?B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
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
<jB
<jB
<jB
<jB
<jB
=qB
=qB
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
A�B
A�B
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
C�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
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
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
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
VB
VB
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
e`B
e`B
ffB
e`B
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
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
o�B
o�B
o�B
o�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B`BbBbBbBbBbBbBaBbBaBaB`B`B`B`BaBaBaB`B`B_B_B\�B[�B_BaBc Bf2BiDBm]Bm]BpoB��B�:B��B��B��B�B��B�B��B��B�vB�~B�xB�kB�eB�_B�4B�B�B��B��B��B~�B~�B�B��B�B�.B�(B�	B�	B{�Bh$Bc BbB^�BU�BL�B9$B2�B0�B �B�B�B�,BΥB�mB�qB{�BncBd&B_BW�BCGB2�B;0B;0B+�B!�B]BxB_BpBjBxB4B
�B
өB
�UB
��B
�vB
�kB
��B
l=B
S�B
AUB
)�B
jB
EB
MB	��B	өB	�"B	�vB	��B	l=B	cB	Y�B	U�B	P�B	L~B	FYB	9$B	2�B	0�B	-�B	(�B	]B	?B	,B	"B	�B��B�tB�vB�QB��BЗB�YB�B��B�pB�jB�]B�YB�SB�B~�Br|Bl=BfBe,BfBdBa�BeB`�BfBi*Bl=B`�B\�BX�BW�BY�BY�BY�B^B\�B]�B^�B^�BY�BT�BS�BKxBJrBIlBHfBFYBESBBABBAB>(B<B:B8B6�B5�B5B2�B2�B3�B/�B/�B/�B/�B.�B-�B,�B+�B+�B)�B'�B'�B&�B'�B&�B&�B%�B%�B#�B$�B#�B"�B#�B"�B%�B%�B$�B&�B%�B%�B&�B'�B'�B'�B'�B)�B(�B)�B+�B+�B+�B+�B+�B*�B(�B)�B(�B)�B*�B+�B*�B*�B*�B*�B+�B-�B.�B.�B.�B/�B0�B3�B2�B1�B0�B3�B4�B8B;B>(B?.B?.B?.B?B?.B>(BBABDMBDMBESBGEBGEBHfBJrBKxBKxBL~BMjBQ�BT�BV�BX�B\�B^�BcBh$Bj0BnIBnIBqABq[BvzBv`BvzBw�B{�B�B��B��B��B��B��B��B��B�B� B�2B�KB�]B�dB�jB�pB��B��B��B��B��B��B��B�	B�(B�AB�MB�SB�9B�?B�XBΊBյB��B�B�[B�hB�hB�nB�ZB��B	�B	�B		�B	B	B	B	B	�B	 B	CB	$tB	&�B	*�B	-�B	0�B	3�B	3�B	5�B	9	B	="B	?.B	@4B	A;B	A;B	A;B	BAB	D3B	G_B	HfB	JrB	KxB	MjB	N�B	P�B	P}B	S�B	V�B	W�B	\�B	`�B	gB	h$B	k6B	kB	l=B	mCB	nIB	pUB	raB	q[B	rGB	tnB	wfB	x�B	z�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�B	�9B	�EB	�EB	�EB	�QB	�=B	�pB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�	B	��B	�B	�B	�B	�"B	�.B	� B	�'B	�GB	�GB	�SB	�KB	�~B	̈́B	̈́B	ΊB	ϑB	ѝB	ңB	ңB	ӏB	өB	ԕB	ԯB	՛B	֡B	خB	��B	خB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�*B	�*B	�6B	�=B	�"B	�CB	�IB	�OB	�OB	�5B	�OB	�UB	�UB	�[B	�[B	�GB	�MB	�nB	�nB	�nB	�nB	�tB	�ZB	�zB	��B	��B	��B	��B	��B	��B	�xB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
 B
B
B
&B
&B
B
&B
,B
2B
2B
2B
B
B
9B
?B
$B
EB
KB
KB
1B
KB
QB
QB
QB
7B
QB
QB
QB
WB
CB
]B
]B
]B
]B
dB
jB
jB
pB
jB
jB
pB
jB
OB
pB
 vB
 vB
 \B
 vB
!|B
!|B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
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
4�B
4�B
3�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
8B
8B
9	B
9	B
8�B
:B
9�B
9�B
9�B
:B
;B
;B
;B
;B
:�B
<B
<B
<B
<B
<B
="B
="B
="B
="B
="B
>(B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
?B
?.B
?.B
?.B
?.B
@4B
@B
@4B
@4B
@4B
@4B
@B
@B
A;B
A;B
A;B
A B
A B
A;B
A;B
A;B
A;B
BAB
BAB
BAB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
DMB
DMB
ESB
ESB
E9B
ESB
E9B
FYB
FYB
F?B
FYB
FYB
GEB
G_B
G_B
HfB
HfB
HfB
HfB
IlB
IRB
HfB
IlB
IlB
IRB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
K^B
KxB
KxB
L~B
L~B
L~B
M�B
M�B
M�B
N�B
NpB
N�B
NpB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
OvB
P�B
P�B
Q�B
Q�B
Q�B
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
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
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
X�B
Y�B
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
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
cB
cB
b�B
dB
dB
dB
dB
dB
eB
eB
d�B
eB
eB
eB
fB
d�B
fB
e�B
fB
fB
fB
fB
gB
gB
gB
gB
h$B
h
B
h$B
h$B
h$B
h$B
h
B
h$B
i*B
i*B
i*B
iB
i*B
j0B
k6B
k6B
k6B
l=B
l=B
l=B
l=B
m)B
mCB
mCB
mCB
mCB
mCB
mCB
mCB
mCB
nIB
nIB
nIB
nIB
nIB
oOB
oOB
oOB
oOB
pU11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.63(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811100037182018111000371820181110003718201811110027582018111100275820181111002758JA  ARFMdecpA19c                                                                20181105123517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181105033541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181105033543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181105033543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181105033544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181105033544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181105033544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181105033544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181105033544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181105033544                      G�O�G�O�G�O�                JA  ARUP                                                                        20181105035617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181105154302  CV  JULD            G�O�G�O�F�r
                JM  ARCAJMQC2.0                                                                 20181109153718  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181109153718  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181110152758  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                