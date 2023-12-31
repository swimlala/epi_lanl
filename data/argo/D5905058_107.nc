CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-30T18:36:34Z creation;2018-11-30T18:36:37Z conversion to V3.1;2019-12-23T06:11:05Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181130183634  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA  I2_0675_107                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؔ���܀1   @ؔ�So�@7G1����cFS��Mj1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @[�@�(�@�(�A
{A*{AJ{Aj{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �HC�HC�HC�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF�HCH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD!�D�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.�RD/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7(RD7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX��DY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe��Df(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq.�Dq�RDr(RDr�RDs(RDs�RDt(RDt�RDu(RDu�RDv(RDv�RDw(RDw�RDx(RDx�RDy(RDy�RDz(RDz�RD{(RD{�RD|(RD|�RD}(RD}�RD~(RD~�RD(RD�RD�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�\D�W\D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D)D��)D�)D�T)DÔ)D��)D�)D�T)DĔ)D��)D�)D�T)DŔ)D��)D�)D�T)DƔ)D��)D�)D�T)Dǔ)D��)D�)D�T)DȔ)D��)D�)D�T)Dɔ)D��)D�)D�T)Dʔ)D��)D�)D�T)D˔)D��)D�)D�T)D̔)D��)D�)D�T)D͔)D��)D�)D�T)DΔ)D��)D�)D�T)Dϔ)D��)D�)D�T)DД)D��)D�)D�T)Dє)D��)D�)D�T)DҔ)D��)D�)D�T)DӔ)D��)D�)D�T)DԔ)D��)D�)D�T)DՔ)D��)D�)D�T)D֔)D��)D�)D�T)Dה)D��)D�)D�T)Dؔ)D��)D�)D�T)Dٔ)D��)D�)D�T)Dڔ)D��)D�)D�T)D۔)D��)D�)D�T)Dܔ)D��)D�)D�T)Dݔ)D��)D�)D�T)Dޔ)D��)D�)D�T)Dߔ)D��)D�)D�T)D��)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�]�D�}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA���A�-A�G�A�S�A�XA�\)A�\)A�\)A�^5A�`BA�`BA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�hsA�hsA�jA�jA�jA�l�A�l�A�n�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�v�A�x�A�v�A�v�A�jA�ZA�&�A��A�x�A�XA���A��TA���A�;dA��hA�ffA���A�l�A��A�1A�^5A�M�A�1'A�/A�"�A���A�|�A�oA��A�\)A���A�A��A�x�A�ƨA��!A���A���A�hsA�ȴA�I�A���A�\)A�bA��A��A���A��wA�z�A��wA�1A��A�&�A�x�A��HA��A��-A�O�A���A�VA�ȴA�\)A�O�A��A�^5A��A�dZA��A���A��A��wA���A��wA���A���AK�A}��A{��Ay�;AwK�At(�Aq��Apz�Ap9XAo��Am�Ai&�Ag�AgS�Af�Ad��Ad-Ac�Ab�HA`��A`M�A_dZA^bNA]��A[`BAW��AV9XASp�AN��AL�uAJM�AH^5AF��AFI�AC�AB~�AB1AA%A@(�A?t�A=��A;��A:�uA9��A81A6�uA5��A5��A57LA4~�A2~�A0ffA/p�A-\)A,5?A+��A*5?A)/A(r�A'&�A%��A$�/A$1'A"�A��A  A��A�A�!A(�A�AVA�AK�A�Ar�A�Ap�A�-A�
A��A�mA%AdZA�/AA�A�A�AS�A
z�A
�A
  A
  A	�A��A�Ap�A\)A%A��A-A�\A�;A�jAA"�A �yA M�@���@�S�@��\@���@��;@���@���@�  @�J@�O�@�bN@�|�@�ȴ@�@�V@�1@���@�X@�@�F@���@�u@�!@�I�@ް!@ݙ�@�X@�&�@ܛ�@���@ٙ�@ץ�@��@֟�@թ�@�A�@�t�@��H@�5?@Л�@�K�@�%@�\)@�E�@�`B@�1@�dZ@�v�@��@���@ŉ7@�/@���@� �@�&�@��@��D@���@�@��@��`@���@�l�@��@�M�@���@��`@�bN@��@��@�-@�/@�9X@���@��H@�~�@�-@��@��D@���@��!@�=q@�X@���@�Q�@�"�@�V@���@��-@��@��@�x�@�%@���@�1@���@��P@�K�@�^5@��@��h@��7@�hs@�?}@���@��@��/@��j@�r�@�9X@��m@���@�|�@�K�@��@��@��H@�ȴ@���@���@���@�$�@��@�@���@�&�@���@�(�@��
@�ƨ@�t�@�S�@�33@���@��y@���@��!@�v�@�ff@�E�@�=q@��@��@���@�V@���@�Z@�1'@�b@��w@�S�@�"�@��R@��@�/@��9@�b@��w@�;d@�"�@�
=@��R@�=q@�@�@��#@���@��@���@��@��D@�z�@�I�@�9X@� �@��@�S�@�K�@�C�@�33@���@�n�@�@�@���@�@��7@�G�@�Ĝ@��D@�r�@�Z@�1'@�b@�b@��@�(�@�r�@�l�@��@��-@��h@��@�J@��@�@���@��7@�G�@�@���@�p�@�G�@��`@��u@�z�@�bN@�Q�@�1'@��@���@��m@���@�S�@�33@�;d@��@���@��y@�v�@�-@���@��-@��7@��@��@�V@�Ĝ@�Q�@�(�@��@�1@��
@��@���@�l�@�@��R@�M�@��@�{@���@���@��T@��^@�hs@�7L@�V@���@��u@�9X@��;@�|�@�K�@�+@��@��y@��H@���@�^5@��@�x�@�X@�O�@�7L@�V@��j@���@��D@��@�z�@�j@�bN@�1'@�@K�@�@~��@~��@~V@}�T@}�-@}�@}V@|�D@{��@{ƨ@{��@{�@{dZ@{C�@{"�@z�H@zM�@yx�@x��@x�@x �@w�@w|�@w�@v�y@v�+@u�@u�h@u�@t�/@t��@t�D@tI�@t9X@s��@s��@r��@q�@qX@p��@p�9@p�@pbN@pbN@pbN@p  @n��@nE�@n{@m��@m�@m��@mO�@l�@l�D@l9X@l1@k�@j�@jJ@i7L@h1'@g�@g��@g�@g|�@g
=@f�+@fV@fE�@f$�@e��@e��@e`B@d�@d�@c�
@cdZ@b�@b��@bM�@a��@aX@a%@`bN@_�;@_�@^��@^��@^E�@]@]�@]?}@\�/@\I�@[��@[ƨ@[C�@Z��@ZM�@Y�@Y��@Y�7@YG�@X�`@X�9@XA�@X  @W�;@W��@W\)@W�@V�@Vȴ@V��@V��@V�+@V5?@V{@V@U�@U��@Up�@U�@TI�@T�@T(�@S��@S��@S�F@S�@SS�@So@R��@R�!@R��@R��@R��@R�\@Rn�@RM�@R�@Q7L@P�9@Pr�@PQ�@P1'@Pb@O�w@O��@O�P@OK�@N�R@N��@N�+@NV@N$�@M��@M�@MO�@M/@L��@L��@L�@LI�@K�m@K�@Ko@J�\@I��@I�7@Ihs@IX@I7L@H�9@Hr�@HbN@Hb@G�;@G�w@G�P@GK�@F�R@Fff@FE�@F$�@E�T@E`B@E?}@D�/@D�j@D�@D�@Dz�@D9X@D(�@C��@Cƨ@C��@B�@BJ@A�@A�#@A�^@A7L@@�`@@Ĝ@@�9@@��@@�@@bN@@1'@@b@?��@?��@?��@?�w@?l�@>��@>�@>�R@>E�@>{@=�T@=��@=/@<��@<�j@<j@;��@;��@;dZ@;S�@;C�@;"�@:�@:��@:��@:^5@:-@:J@9��@9��@9G�@9�@8�`@8r�@8A�@8 �@7�@7�@6v�@5�@5��@5?}@4��@4j@3�m@3�F@3��@3"�@2�!@2��@2�\@2�\@2^5@2J@1��@1�#@1��@1&�@0�@0r�@0bN@0 �@/�@/�w@/�P@/
=@.�+@.5?@.{@-�T@-@-�-@-�h@-?}@,�j@,�@,�D@,(�@+ƨ@+��@+�@+C�@+o@+@+@*�@*�H@*��@*~�@)��@)�^@)x�@)7L@(��@(�u@(�@(bN@'�;@'�w@'�@'��@'|�@'l�@'\)@'�@'
=@'
=@&�+@&V@&V@&E�@&$�@%�-@%�@%p�@%p�@%O�@%/@$�/@$�j@$j@$I�@$�@#��@#dZ@#33@"�@"��@"�!@"�!@"~�@"~�@"~�@"^5@!�@!hs@ �`@ ��@ r�@ Q�@ A�@   @��@|�@+@�@ȴ@ȴ@�R@v�@5?@�@p�@�@��@I�@��@��@dZ@"�@�@�H@��@�!@�\@n�@^5@M�@M�@-@�@�@��@hs@7L@&�@%@�`@�`@Ĝ@��@�u@r�@bN@b@  @�;@�w@�P@K�@�@��@�+@{@�@p�@�@��@�D@Z@9X@�@�
@�@t�@dZ@C�@�@�!@^5@M�@M�@M�@=q@J@�@�#@��@X@7L@&�@&�@Ĝ@Q�@1'@ �@�@�w@��@�P@|�@l�@\)@;d@;d@+@
=@ȴ@��@�+@ff@V@$�@@��@�@p�@O�@?}@?}@/@��@�j@9X@�
@��@t�@t�@dZ@C�@"�@@
��@
=q@
J@	�#@	�^@	��@	x�@	7L@��@�@r�@b@�w@�@��@��@��@|�@l�@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ĜA���A�-A�G�A�S�A�XA�\)A�\)A�\)A�^5A�`BA�`BA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�hsA�hsA�jA�jA�jA�l�A�l�A�n�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�t�A�t�A�v�A�x�A�v�A�v�A�jA�ZA�&�A��A�x�A�XA���A��TA���A�;dA��hA�ffA���A�l�A��A�1A�^5A�M�A�1'A�/A�"�A���A�|�A�oA��A�\)A���A�A��A�x�A�ƨA��!A���A���A�hsA�ȴA�I�A���A�\)A�bA��A��A���A��wA�z�A��wA�1A��A�&�A�x�A��HA��A��-A�O�A���A�VA�ȴA�\)A�O�A��A�^5A��A�dZA��A���A��A��wA���A��wA���A���AK�A}��A{��Ay�;AwK�At(�Aq��Apz�Ap9XAo��Am�Ai&�Ag�AgS�Af�Ad��Ad-Ac�Ab�HA`��A`M�A_dZA^bNA]��A[`BAW��AV9XASp�AN��AL�uAJM�AH^5AF��AFI�AC�AB~�AB1AA%A@(�A?t�A=��A;��A:�uA9��A81A6�uA5��A5��A57LA4~�A2~�A0ffA/p�A-\)A,5?A+��A*5?A)/A(r�A'&�A%��A$�/A$1'A"�A��A  A��A�A�!A(�A�AVA�AK�A�Ar�A�Ap�A�-A�
A��A�mA%AdZA�/AA�A�A�AS�A
z�A
�A
  A
  A	�A��A�Ap�A\)A%A��A-A�\A�;A�jAA"�A �yA M�@���@�S�@��\@���@��;@���@���@�  @�J@�O�@�bN@�|�@�ȴ@�@�V@�1@���@�X@�@�F@���@�u@�!@�I�@ް!@ݙ�@�X@�&�@ܛ�@���@ٙ�@ץ�@��@֟�@թ�@�A�@�t�@��H@�5?@Л�@�K�@�%@�\)@�E�@�`B@�1@�dZ@�v�@��@���@ŉ7@�/@���@� �@�&�@��@��D@���@�@��@��`@���@�l�@��@�M�@���@��`@�bN@��@��@�-@�/@�9X@���@��H@�~�@�-@��@��D@���@��!@�=q@�X@���@�Q�@�"�@�V@���@��-@��@��@�x�@�%@���@�1@���@��P@�K�@�^5@��@��h@��7@�hs@�?}@���@��@��/@��j@�r�@�9X@��m@���@�|�@�K�@��@��@��H@�ȴ@���@���@���@�$�@��@�@���@�&�@���@�(�@��
@�ƨ@�t�@�S�@�33@���@��y@���@��!@�v�@�ff@�E�@�=q@��@��@���@�V@���@�Z@�1'@�b@��w@�S�@�"�@��R@��@�/@��9@�b@��w@�;d@�"�@�
=@��R@�=q@�@�@��#@���@��@���@��@��D@�z�@�I�@�9X@� �@��@�S�@�K�@�C�@�33@���@�n�@�@�@���@�@��7@�G�@�Ĝ@��D@�r�@�Z@�1'@�b@�b@��@�(�@�r�@�l�@��@��-@��h@��@�J@��@�@���@��7@�G�@�@���@�p�@�G�@��`@��u@�z�@�bN@�Q�@�1'@��@���@��m@���@�S�@�33@�;d@��@���@��y@�v�@�-@���@��-@��7@��@��@�V@�Ĝ@�Q�@�(�@��@�1@��
@��@���@�l�@�@��R@�M�@��@�{@���@���@��T@��^@�hs@�7L@�V@���@��u@�9X@��;@�|�@�K�@�+@��@��y@��H@���@�^5@��@�x�@�X@�O�@�7L@�V@��j@���@��D@��@�z�@�j@�bN@�1'@�@K�@�@~��@~��@~V@}�T@}�-@}�@}V@|�D@{��@{ƨ@{��@{�@{dZ@{C�@{"�@z�H@zM�@yx�@x��@x�@x �@w�@w|�@w�@v�y@v�+@u�@u�h@u�@t�/@t��@t�D@tI�@t9X@s��@s��@r��@q�@qX@p��@p�9@p�@pbN@pbN@pbN@p  @n��@nE�@n{@m��@m�@m��@mO�@l�@l�D@l9X@l1@k�@j�@jJ@i7L@h1'@g�@g��@g�@g|�@g
=@f�+@fV@fE�@f$�@e��@e��@e`B@d�@d�@c�
@cdZ@b�@b��@bM�@a��@aX@a%@`bN@_�;@_�@^��@^��@^E�@]@]�@]?}@\�/@\I�@[��@[ƨ@[C�@Z��@ZM�@Y�@Y��@Y�7@YG�@X�`@X�9@XA�@X  @W�;@W��@W\)@W�@V�@Vȴ@V��@V��@V�+@V5?@V{@V@U�@U��@Up�@U�@TI�@T�@T(�@S��@S��@S�F@S�@SS�@So@R��@R�!@R��@R��@R��@R�\@Rn�@RM�@R�@Q7L@P�9@Pr�@PQ�@P1'@Pb@O�w@O��@O�P@OK�@N�R@N��@N�+@NV@N$�@M��@M�@MO�@M/@L��@L��@L�@LI�@K�m@K�@Ko@J�\@I��@I�7@Ihs@IX@I7L@H�9@Hr�@HbN@Hb@G�;@G�w@G�P@GK�@F�R@Fff@FE�@F$�@E�T@E`B@E?}@D�/@D�j@D�@D�@Dz�@D9X@D(�@C��@Cƨ@C��@B�@BJ@A�@A�#@A�^@A7L@@�`@@Ĝ@@�9@@��@@�@@bN@@1'@@b@?��@?��@?��@?�w@?l�@>��@>�@>�R@>E�@>{@=�T@=��@=/@<��@<�j@<j@;��@;��@;dZ@;S�@;C�@;"�@:�@:��@:��@:^5@:-@:J@9��@9��@9G�@9�@8�`@8r�@8A�@8 �@7�@7�@6v�@5�@5��@5?}@4��@4j@3�m@3�F@3��@3"�@2�!@2��@2�\@2�\@2^5@2J@1��@1�#@1��@1&�@0�@0r�@0bN@0 �@/�@/�w@/�P@/
=@.�+@.5?@.{@-�T@-@-�-@-�h@-?}@,�j@,�@,�D@,(�@+ƨ@+��@+�@+C�@+o@+@+@*�@*�H@*��@*~�@)��@)�^@)x�@)7L@(��@(�u@(�@(bN@'�;@'�w@'�@'��@'|�@'l�@'\)@'�@'
=@'
=@&�+@&V@&V@&E�@&$�@%�-@%�@%p�@%p�@%O�@%/@$�/@$�j@$j@$I�@$�@#��@#dZ@#33@"�@"��@"�!@"�!@"~�@"~�@"~�@"^5@!�@!hs@ �`@ ��@ r�@ Q�@ A�@   @��@|�@+@�@ȴ@ȴ@�R@v�@5?@�@p�@�@��@I�@��@��@dZ@"�@�@�H@��@�!@�\@n�@^5@M�@M�@-@�@�@��@hs@7L@&�@%@�`@�`@Ĝ@��@�u@r�@bN@b@  @�;@�w@�P@K�@�@��@�+@{@�@p�@�@��@�D@Z@9X@�@�
@�@t�@dZ@C�@�@�!@^5@M�@M�@M�@=q@J@�@�#@��@X@7L@&�@&�@Ĝ@Q�@1'@ �@�@�w@��@�P@|�@l�@\)@;d@;d@+@
=@ȴ@��@�+@ff@V@$�@@��@�@p�@O�@?}@?}@/@��@�j@9X@�
@��@t�@t�@dZ@C�@"�@@
��@
=q@
J@	�#@	�^@	��@	x�@	7L@��@�@r�@b@�w@�@��@��@��@|�@l�@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B7LBO�Bo�B~�B�%B�+B�1B�1B�1B�1B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�7B�=B�7B�7B�7B�7B�1B�B�VB��B�hB�oB��B��B��B��B��B�bB�%B}�B{�B{�Bq�Bo�Bo�Bn�Bn�BjBgmBbNB_;B]/B[#BVBT�BYBXBVBN�B@�B49B0!B)�B(�B%�B�BuBbBVB
=BB��B�yB�BɺB��B�3B�B��B��BjBN�BE�B49BB
�fB
�/B
�
B
��B
�XB
��B
�+B
�=B
�oB
�B
�B
gmB
R�B
B�B
6FB
$�B
PB	��B	�B	�sB	�TB	�
B	�qB	��B	��B	��B	�hB	�JB	��B	�\B	�B	� B	y�B	q�B	iyB	\)B	@�B	2-B	$�B	B��B�B�)B��B��BĜB��BŢB��B��B��BɺB��B�^B�FB�3B�-B�B�B�B��B��B�\B�JB�7B� B~�B�B{�Bx�Bv�Bq�Bm�Bl�BiyBffB]/B\)BXBS�BS�BP�BO�BL�BJ�BI�BH�BF�BD�BB�B>wB:^B8RB9XB6FB5?B33B1'B1'B/B/B-B-B,B.B(�B(�B'�B'�B'�B'�B%�B%�B#�B#�B"�B �B�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B%�B!�B#�B'�B(�B(�B+B.B.B1'B1'B0!B2-B33B49B33B49B6FB6FB;dB=qB>wB?}BB�BB�BD�BD�BE�BE�BF�BF�BG�BP�BO�BP�BQ�BS�BW
BZB]/B^5B`BBaHBcTBffBgmBjBk�Bo�Bs�Bw�By�B{�B|�B~�B� B�B�7B�=B�JB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�RB�jB�jB�qB�}BBĜBƨBɺB��B��B��B�B�)B�BB�TB�fB�mB�sB�yB�B�B��B��B��B��B	  B	B		7B	PB	hB	{B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	#�B	%�B	'�B	+B	0!B	2-B	5?B	6FB	7LB	;dB	?}B	?}B	B�B	F�B	K�B	N�B	O�B	Q�B	S�B	T�B	T�B	VB	YB	[#B	\)B	]/B	]/B	_;B	`BB	aHB	cTB	e`B	gmB	hsB	iyB	jB	n�B	q�B	r�B	s�B	v�B	x�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�DB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�jB	�qB	�qB	�}B	��B	��B	B	ĜB	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
%B
+B
1B
	7B

=B
DB
JB
JB
PB
PB
VB
VB
VB
VB
VB
bB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
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
(�B
(�B
)�B
+B
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
/B
0!B
0!B
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
33B
33B
33B
49B
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
6FB
6FB
7LB
7LB
7LB
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
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
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
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
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
K�B
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
Q�B
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
XB
XB
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
bNB
bNB
bNB
bNB
bNB
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
ffB
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
iyB
iyB
iyB
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
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B7BO�BoiB~�B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�	B�B�	B�B�B�B�B��B��B�"B�MB�4B�:B�eB�eB�_B��B�SB�.B��B}�B{�B{�BqvBoiBoiBncBncBjKBg8BbB_B\�BZ�BU�BT�BX�BW�BU�BN�B@4B4B/�B)�B(�B%�B]B@B.BB
	B�B��B�*BյBɆB�OB��B��B��B�KBjKBN�BESB3�B �B
�B
��B
��B
ϫB
�$B
�KB
��B
��B
�:B
��B
��B
g8B
R�B
B[B
6B
$�B
B	��B	�]B	�$B	�B	ּB	�<B	��B	��B	��B	�4B	��B	�2B	�B	��B	�B	y�B	q[B	iDB	[�B	@4B	1�B	$�B	�B�zB�QB��BϑBΊB�MB�4B�SBөBЗBΊB�lB�;B�*B��B��B��B��B��B��B��B�dB�(B��B��B�B~�B��B{�Bx�BvzBq[Bm]Bl=BiDBfB\�B[�BW�BS�BS�BP�BO�BL~BJrBIlBHfBFYBDMBB[B>(B:B8B9$B5�B4�B2�B0�B0�B.�B.�B,�B,�B+�B-�B(�B(�B'�B'�B'�B'�B%�B%�B#�B#�B"�B �BpB vB vBdBdBdBdBjBdBdB�BjBjBdBWB]BQB]B]B]B]BxB vB$�B%�B!|B#�B'�B(�B(�B*�B-�B-�B0�B0�B/�B1�B2�B3�B2�B3�B5�B5�B;B="B>B?.BBABBABD3BDMBESBESBFYBFYBG_BP�BO�BP�BQ�BS�BV�BY�B\�B]�B_�B`�BcBfBgBj0Bk6BoOBshBw�By�B{�B|�B~�B�B��B��B��B��B�B�B�B�KB�CB�jB�pB�|B�|B�|B�nB��B��B��B��B��B��B��B�B�B�"B�.B�AB�MB�?B�lB�~BЗBԯBخB��B��B�B�B�B�$B�*B�B�IB�tB�lB��B��B��B	�B	�B	B	B	,B	9B	EB	=B	]B	dB	OB	!|B	!|B	#nB	#�B	%�B	'�B	*�B	/�B	1�B	4�B	5�B	6�B	;B	?.B	?.B	BAB	F?B	KxB	N�B	O�B	Q�B	S�B	T�B	T�B	U�B	X�B	Z�B	[�B	\�B	\�B	^�B	_�B	`�B	cB	d�B	gB	h
B	i*B	j0B	nIB	qAB	raB	shB	vzB	x�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�KB	�B	�?B	�EB	�]B	�pB	�|B	�|B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�"B	�B	�.B	�4B	� B	�AB	�3B	�SB	�fB	�fB	�RB	�xB	�xB	�^B	�~B	�~B	̈́B	ΊB	�pB	�vB	ЗB	ЗB	ңB	ӏB	ԯB	֡B	רB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�*B	�6B	�6B	�6B	�6B	�0B	�=B	�)B	�CB	�CB	�CB	�IB	�OB	�OB	�;B	�AB	�AB	�[B	�aB	�hB	�MB	�nB	�nB	�nB	�tB	�`B	�`B	�`B	��B	�fB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
�B
B
B
B
B
B
B
B
B
B
 B
 B
 B
 B
&B
B
2B
2B
B
2B
2B
2B
2B
9B
?B
$B
?B
+B
EB
KB
KB
QB
QB
WB
WB
WB
]B
CB
dB
dB
IB
jB
jB
jB
pB
pB
pB
 vB
 vB
!|B
!|B
!|B
!|B
"�B
"hB
"�B
"hB
"�B
#�B
#�B
#�B
#nB
#�B
$tB
$�B
$�B
$�B
$�B
$tB
$�B
$�B
%�B
%zB
&�B
&�B
&�B
&�B
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
(�B
(�B
)�B
*�B
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
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
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
2�B
2�B
2�B
3�B
3�B
3�B
3�B
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
6�B
6�B
8B
9	B
9	B
8�B
9	B
9	B
9�B
:B
9�B
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
<B
<B
="B
="B
="B
=B
="B
=B
>(B
>B
?.B
?.B
?.B
?.B
?.B
?.B
?B
?.B
?.B
?B
@4B
@4B
@B
@B
@B
A;B
A;B
A B
A B
BAB
BAB
CGB
CGB
C-B
DMB
ESB
ESB
ESB
ESB
FYB
FYB
F?B
FYB
G_B
G_B
G_B
G_B
GEB
GEB
HfB
IlB
IRB
IlB
IlB
IlB
IlB
IlB
JrB
JrB
JXB
K^B
K^B
KxB
KxB
K^B
KxB
LdB
L~B
L~B
M�B
MjB
M�B
M�B
M�B
NpB
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
P}B
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
Q�B
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
W�B
W�B
V�B
V�B
V�B
V�B
W�B
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
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
cB
cB
c�B
dB
dB
c�B
dB
d�B
eB
d�B
eB
d�B
eB
e�B
fB
fB
e�B
e�B
fB
fB
e�B
fB
gB
gB
gB
gB
gB
h$B
h$B
h
B
h$B
h$B
h$B
iB
i*B
i*B
i*B
i*B
i*B
i*B
i*B
i*B
i*B
j0B
jB
j0B
jB
j0B
j0B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
k6B
kB
l=B
l=B
mCB
mCB
mCB
mCB
mCB
m)B
mCB
mCB
nIB
nIB
nIB
n/B
nIB
nIB
oOB
oOB
o5B
oOB
pUB
pUB
p;B
pUB
pUB
pUB
p;B
pUB
q[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.63(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812060051262018120600512620181206005126201812070034592018120700345920181207003459JA  ARFMdecpA19c                                                                20181201033620  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181130183634  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181130183635  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181130183635  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181130183636  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181130183636  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181130183636  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181130183636  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181130183637  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181130183637                      G�O�G�O�G�O�                JA  ARUP                                                                        20181130185617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181201154110  CV  JULD            G�O�G�O�Fĥ6                JM  ARCAJMQC2.0                                                                 20181205155126  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181205155126  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181206153459  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                