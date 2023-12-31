CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-06T06:35:15Z creation;2018-10-06T06:35:18Z conversion to V3.1;2019-12-23T06:13:57Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181006063515  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ^A   JA  I2_0675_094                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؆�:�v�1   @؆���J @7:)�y��cXu%F1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ D�|�Dļ�D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�C3DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�{@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B
\)B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Bb\)Bj\)Bq�\By�\B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ǮB���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
c�C}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,�
C.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-�D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6%�D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX��DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�L{D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�DÏ�D�ϮD��D�O�DČ{D��{D�{D�O�Dŏ�D�ϮD��D�O�DƏ�D�ϮD��D�O�DǏ�D�ϮD��D�O�Dȏ�D�ϮD��D�O�Dɏ�D�ϮD��D�O�Dʏ�D�ϮD��D�O�Dˏ�D�ϮD��D�O�D̏�D�ϮD��D�O�D͏�D�ϮD��D�O�DΏ�D�ϮD��D�O�DϏ�D�ϮD��D�O�DЏ�D�ϮD��D�O�Dя�D�ϮD��D�O�Dҏ�D�ϮD��D�R�Dӏ�D�ϮD��D�O�Dԏ�D�ϮD��D�O�DՏ�D�ϮD��D�O�D֏�D�ϮD��D�O�D׏�D�ϮD��D�O�D؏�D�ϮD��D�O�Dُ�D�ϮD��D�O�Dڏ�D�ϮD��D�O�Dۏ�D�ϮD��D�O�D܏�D�ϮD��D�O�Dݏ�D�ϮD��D�O�Dޏ�D�ϮD��D�O�Dߏ�D�ϮD��D�O�D���D�ϮD��D�O�DᏮD�ϮD��D�O�D⏮D�ϮD��D�O�D㏮D�ϮD��D�O�D䏮D�ϮD��D�O�D叮D�ϮD��D�O�D揮D�ϮD��D�O�D珮D�ϮD��D�O�D菮D�ϮD��D�O�D鏮D�ϮD��D�O�DꏮD�ϮD��D�O�D돮D�ϮD��D�O�D쏮D�ϮD��D�O�D폮D�ϮD��D�O�DD�ϮD��D�O�DD�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D��D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA�ĜA���A���A��
A���A�ƨAҁA�jA�K�A�+A�1A��yA��#A���A�AѶFAѩ�AхAЛ�A��`Aϴ9AϏ\Aω7AϋDAϾwAυA��;A�v�A�1'A��
A�JA�Q�A��A��A�1'A��
A�hsA��A���A�(�A�\)A�9XA�{A��jA��-A�7LA��TA�S�A���A�\)A��DA�jA���A��!A���A�=qA�(�A�XA��9A��A�JA���A���A�;dA�+A��mA���A�~�A�bNA�A�{A�(�A���A��FA��\A�"�A��A�"�A��mA�`BA��A�bNA�oA��
A�r�A�S�A��;A��hA���A��PA��RA��A��A��yA�ȴA���A�ĜA��A��uA���A��wA�ƨA��`A�A�ffA�#A|�RAzffAy33Ax�!AxQ�Au�TAr(�Aq%Apz�An�Ak�Aj��Ai�#Ait�AhQ�Af�/AeAcoA`-A^$�A\M�AZ$�AW��AV��AU|�AT-ARI�APVAOG�AN�/ANbNAMAL  AJffAHȴAGXAF1AE33AE�AD$�AB�/AA|�A@�uA?��A>n�A<�RA;��A:�A9&�A7`BA6Q�A5��A4VA2�uA1�hA0 �A.�jA,��A+dZA*VA)C�A(JA&��A&(�A%��A%?}A%A$ffA#33A!%A ��A ��A ĜA A�`AM�A��AO�AȴA�mA�AA�A��A�A|�A�RA�hAI�A��A�+A��A��A��AC�A��A�A%A��A�A
z�A	hsA�AdZA%Av�A��A33A�\AM�A�TAx�AC�AȴA��A;dA �A 1'@��P@�^5@�hs@�r�@� �@�33@�E�@��/@��m@�l�@��@��\@���@�ȴ@�p�@�ƨ@�o@�{@���@��@�^@�Z@�ȴ@�7@��@���@�^@�?}@�Q�@߅@���@�33@�v�@���@���@��@�ƨ@֗�@��/@ҸR@�V@��#@���@υ@�n�@�hs@�r�@ˮ@��@�V@�l�@��T@��@�r�@�o@��@��@�t�@��@�n�@�@�bN@�b@���@��@��@�n�@��^@�G�@�Ĝ@�Q�@� �@�@���@�7L@��/@��@��u@�I�@��P@�"�@���@�O�@�C�@��@��!@�V@��@��@�1'@���@��!@�~�@�E�@�hs@�j@�A�@�9X@�1'@�(�@���@�~�@�J@���@���@�x�@�x�@��@��j@�I�@�  @��@���@�t�@�K�@���@��#@�J@�X@��@���@��9@��j@��@�A�@�K�@�@�^5@�@�X@�V@�hs@��#@���@���@���@�G�@�?}@�&�@��`@��@��m@��@�C�@��@��@�~�@��#@��j@�1'@�1'@�A�@�b@��;@��P@�@�@�33@��@��!@��h@��@��@��`@��/@��j@���@�z�@�j@��u@���@��`@��`@��`@�Ĝ@���@�Z@�bN@��D@�z�@�I�@��F@���@�K�@��@�@�
=@�o@��H@�^5@���@���@���@��@�`B@�7L@��j@��u@�bN@�A�@�9X@�(�@���@��;@�
=@�v�@��@�{@�J@�@��@��7@�7L@�&�@��@�V@��`@���@�r�@�b@�ƨ@��P@�l�@�;d@�+@���@��R@���@�^5@�-@�$�@�@��@���@���@�p�@�&�@��/@�j@�A�@�(�@��@��
@���@�K�@��@�~�@�=q@��#@���@��@�`B@�?}@��@���@��9@���@�j@�9X@�1@��w@��@�S�@���@��+@�V@�{@���@���@�hs@�G�@���@��@��D@�j@�Z@�9X@�(�@��@�  @|�@~v�@~5?@~@}�T@}��@}��@}?}@|�@|�@|�/@|��@|I�@{t�@{C�@{o@z�H@z��@z�!@z�@y��@yx�@yx�@yx�@yx�@yhs@yhs@yhs@xr�@x  @w�P@w\)@w;d@v��@u�T@u�h@up�@u�@tI�@s33@s"�@so@r�\@rM�@rJ@q�@q��@qx�@qX@q7L@q%@p�u@o��@n��@n�+@n@m@m�-@m�-@m�h@m`B@m/@l�/@l�j@l(�@k��@k33@j�H@jJ@i��@ihs@h�`@h�u@h�@hr�@hQ�@h1'@hb@g�;@g|�@f�@f5?@e�-@e/@d�@d�@d�@d�D@dj@c�
@cdZ@c@b�!@bM�@bJ@a��@a7L@`�`@`�9@`b@_�@_K�@^ȴ@^5?@^{@^@]�@]�@]@]p�@\��@\�@\1@[��@[C�@[@Z��@Zn�@Y�@Yx�@YG�@Y�@X��@X�@XQ�@X  @W��@W�w@WK�@Vȴ@Vv�@VV@V5?@U�T@U��@U?}@T�j@T��@S�m@St�@S33@R�H@Rn�@R-@Q�@Q�#@Q�#@Q��@Qx�@Qhs@QX@Q%@PA�@O�;@O��@O�P@N�y@Nv�@N5?@M�@M�-@Mp�@M/@L��@L�@Lz�@K��@K�F@Kt�@K33@J�!@J�\@JM�@IX@H�9@G�;@G�P@G�P@G��@G��@G|�@G\)@Fȴ@Fv�@FE�@E�@Ep�@D�@D�D@C��@C��@Ct�@C"�@B�!@BM�@A�@A�^@A��@AG�@AG�@A�@@�`@@��@@bN@@  @?�P@?l�@?+@>ff@>@=��@=�-@=/@<�/@<�D@;ƨ@;dZ@;o@:�@:�H@:��@:�\@:-@9��@9�^@9x�@9hs@9X@97L@8Ĝ@8Q�@8  @7��@7��@7\)@7
=@6��@6ff@6$�@5��@5�@5p�@5p�@5`B@5?}@5�@4�@4��@4Z@4�@3�m@3��@3S�@2�!@2�\@2^5@2-@2J@1�#@1G�@0�9@0��@0Q�@/��@/�P@/\)@.�y@.��@.��@.�+@.5?@-�@-�@-p�@-�@-�@-�h@-�h@-p�@-/@-/@-V@,��@,�@,z�@,9X@,1@+��@+�@+�@+t�@+o@*�@*�@*�!@*�\@*~�@*M�@*�@*J@*J@*J@*J@*J@)��@)��@)X@)G�@)�@(Ĝ@(�9@(�9@(A�@(  @'�;@'�w@'��@';d@&��@&ȴ@&��@&�+@&�+@&v�@%�T@%O�@%/@%/@$�@$Z@#�F@#C�@"�H@"��@"��@"��@"�!@"�\@"=q@!�@!��@!�7@!hs@!G�@!7L@ ��@ r�@ A�@   @�@|�@|�@\)@K�@;d@
=@�@ȴ@�R@��@v�@{@@p�@/@V@z�@�
@t�@o@@@@�H@��@~�@n�@^5@-@J@��@�7@hs@X@Ĝ@r�@Q�@ �@�P@+@�+@ff@V@E�@�T@�@O�@?}@/@�@�/@�j@�j@�j@�j@�@��@j@9X@1@�m@��@S�@�H@~�@=q@-@J@��@�@�^@�7@x�@hs@7L@%@��@��@�@�@�@K�@�@�@��@v�@E�@5?@$�@�T@@�h@�h@`B@/@��@�j@�D@Z@(�@ƨ@t�@S�@S�@33@
�H@
�\@
�\@
�\@
~�@
n�@
M�@
J@	�@	�#@	��@	7L@	�@	%@��@�9@A�@ �@b@  @�;@�w@�w@|�@l�@\)@;d@�@ȴ@V@5?@{@�@��@@�-@�-@�-@��@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA�ĜA���A���A��
A���A�ƨAҁA�jA�K�A�+A�1A��yA��#A���A�AѶFAѩ�AхAЛ�A��`Aϴ9AϏ\Aω7AϋDAϾwAυA��;A�v�A�1'A��
A�JA�Q�A��A��A�1'A��
A�hsA��A���A�(�A�\)A�9XA�{A��jA��-A�7LA��TA�S�A���A�\)A��DA�jA���A��!A���A�=qA�(�A�XA��9A��A�JA���A���A�;dA�+A��mA���A�~�A�bNA�A�{A�(�A���A��FA��\A�"�A��A�"�A��mA�`BA��A�bNA�oA��
A�r�A�S�A��;A��hA���A��PA��RA��A��A��yA�ȴA���A�ĜA��A��uA���A��wA�ƨA��`A�A�ffA�#A|�RAzffAy33Ax�!AxQ�Au�TAr(�Aq%Apz�An�Ak�Aj��Ai�#Ait�AhQ�Af�/AeAcoA`-A^$�A\M�AZ$�AW��AV��AU|�AT-ARI�APVAOG�AN�/ANbNAMAL  AJffAHȴAGXAF1AE33AE�AD$�AB�/AA|�A@�uA?��A>n�A<�RA;��A:�A9&�A7`BA6Q�A5��A4VA2�uA1�hA0 �A.�jA,��A+dZA*VA)C�A(JA&��A&(�A%��A%?}A%A$ffA#33A!%A ��A ��A ĜA A�`AM�A��AO�AȴA�mA�AA�A��A�A|�A�RA�hAI�A��A�+A��A��A��AC�A��A�A%A��A�A
z�A	hsA�AdZA%Av�A��A33A�\AM�A�TAx�AC�AȴA��A;dA �A 1'@��P@�^5@�hs@�r�@� �@�33@�E�@��/@��m@�l�@��@��\@���@�ȴ@�p�@�ƨ@�o@�{@���@��@�^@�Z@�ȴ@�7@��@���@�^@�?}@�Q�@߅@���@�33@�v�@���@���@��@�ƨ@֗�@��/@ҸR@�V@��#@���@υ@�n�@�hs@�r�@ˮ@��@�V@�l�@��T@��@�r�@�o@��@��@�t�@��@�n�@�@�bN@�b@���@��@��@�n�@��^@�G�@�Ĝ@�Q�@� �@�@���@�7L@��/@��@��u@�I�@��P@�"�@���@�O�@�C�@��@��!@�V@��@��@�1'@���@��!@�~�@�E�@�hs@�j@�A�@�9X@�1'@�(�@���@�~�@�J@���@���@�x�@�x�@��@��j@�I�@�  @��@���@�t�@�K�@���@��#@�J@�X@��@���@��9@��j@��@�A�@�K�@�@�^5@�@�X@�V@�hs@��#@���@���@���@�G�@�?}@�&�@��`@��@��m@��@�C�@��@��@�~�@��#@��j@�1'@�1'@�A�@�b@��;@��P@�@�@�33@��@��!@��h@��@��@��`@��/@��j@���@�z�@�j@��u@���@��`@��`@��`@�Ĝ@���@�Z@�bN@��D@�z�@�I�@��F@���@�K�@��@�@�
=@�o@��H@�^5@���@���@���@��@�`B@�7L@��j@��u@�bN@�A�@�9X@�(�@���@��;@�
=@�v�@��@�{@�J@�@��@��7@�7L@�&�@��@�V@��`@���@�r�@�b@�ƨ@��P@�l�@�;d@�+@���@��R@���@�^5@�-@�$�@�@��@���@���@�p�@�&�@��/@�j@�A�@�(�@��@��
@���@�K�@��@�~�@�=q@��#@���@��@�`B@�?}@��@���@��9@���@�j@�9X@�1@��w@��@�S�@���@��+@�V@�{@���@���@�hs@�G�@���@��@��D@�j@�Z@�9X@�(�@��@�  @|�@~v�@~5?@~@}�T@}��@}��@}?}@|�@|�@|�/@|��@|I�@{t�@{C�@{o@z�H@z��@z�!@z�@y��@yx�@yx�@yx�@yx�@yhs@yhs@yhs@xr�@x  @w�P@w\)@w;d@v��@u�T@u�h@up�@u�@tI�@s33@s"�@so@r�\@rM�@rJ@q�@q��@qx�@qX@q7L@q%@p�u@o��@n��@n�+@n@m@m�-@m�-@m�h@m`B@m/@l�/@l�j@l(�@k��@k33@j�H@jJ@i��@ihs@h�`@h�u@h�@hr�@hQ�@h1'@hb@g�;@g|�@f�@f5?@e�-@e/@d�@d�@d�@d�D@dj@c�
@cdZ@c@b�!@bM�@bJ@a��@a7L@`�`@`�9@`b@_�@_K�@^ȴ@^5?@^{@^@]�@]�@]@]p�@\��@\�@\1@[��@[C�@[@Z��@Zn�@Y�@Yx�@YG�@Y�@X��@X�@XQ�@X  @W��@W�w@WK�@Vȴ@Vv�@VV@V5?@U�T@U��@U?}@T�j@T��@S�m@St�@S33@R�H@Rn�@R-@Q�@Q�#@Q�#@Q��@Qx�@Qhs@QX@Q%@PA�@O�;@O��@O�P@N�y@Nv�@N5?@M�@M�-@Mp�@M/@L��@L�@Lz�@K��@K�F@Kt�@K33@J�!@J�\@JM�@IX@H�9@G�;@G�P@G�P@G��@G��@G|�@G\)@Fȴ@Fv�@FE�@E�@Ep�@D�@D�D@C��@C��@Ct�@C"�@B�!@BM�@A�@A�^@A��@AG�@AG�@A�@@�`@@��@@bN@@  @?�P@?l�@?+@>ff@>@=��@=�-@=/@<�/@<�D@;ƨ@;dZ@;o@:�@:�H@:��@:�\@:-@9��@9�^@9x�@9hs@9X@97L@8Ĝ@8Q�@8  @7��@7��@7\)@7
=@6��@6ff@6$�@5��@5�@5p�@5p�@5`B@5?}@5�@4�@4��@4Z@4�@3�m@3��@3S�@2�!@2�\@2^5@2-@2J@1�#@1G�@0�9@0��@0Q�@/��@/�P@/\)@.�y@.��@.��@.�+@.5?@-�@-�@-p�@-�@-�@-�h@-�h@-p�@-/@-/@-V@,��@,�@,z�@,9X@,1@+��@+�@+�@+t�@+o@*�@*�@*�!@*�\@*~�@*M�@*�@*J@*J@*J@*J@*J@)��@)��@)X@)G�@)�@(Ĝ@(�9@(�9@(A�@(  @'�;@'�w@'��@';d@&��@&ȴ@&��@&�+@&�+@&v�@%�T@%O�@%/@%/@$�@$Z@#�F@#C�@"�H@"��@"��@"��@"�!@"�\@"=q@!�@!��@!�7@!hs@!G�@!7L@ ��@ r�@ A�@   @�@|�@|�@\)@K�@;d@
=@�@ȴ@�R@��@v�@{@@p�@/@V@z�@�
@t�@o@@@@�H@��@~�@n�@^5@-@J@��@�7@hs@X@Ĝ@r�@Q�@ �@�P@+@�+@ff@V@E�@�T@�@O�@?}@/@�@�/@�j@�j@�j@�j@�@��@j@9X@1@�m@��@S�@�H@~�@=q@-@J@��@�@�^@�7@x�@hs@7L@%@��@��@�@�@�@K�@�@�@��@v�@E�@5?@$�@�T@@�h@�h@`B@/@��@�j@�D@Z@(�@ƨ@t�@S�@S�@33@
�H@
�\@
�\@
�\@
~�@
n�@
M�@
J@	�@	�#@	��@	7L@	�@	%@��@�9@A�@ �@b@  @�;@�w@�w@|�@l�@\)@;d@�@ȴ@V@5?@{@�@��@@�-@�-@�-@��@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B'�B'�B&�B&�B&�B&�B&�B)�B(�B(�B)�B+B+B+B)�B(�B'�B&�B#�BhBBBB	7BuB$�B,B0!B]/Bn�B� B�oB��B�B�B�!B�B�B�B�B�'B�XB�RB�LB�LB�LB�LB�3B�-B�!B�B��B��B�RB�9B��B��B��B��B��B��B��B�{B�\B�+B�%B~�Bw�BgmBbNB^5BT�BH�B@�B@�B=qB8RB"�BB�B�fB�;B�B�TB��BɺBǮB�9B��Br�BP�B49BbB  B
��B
��B
��B
�B
�)B
ȴB
��B
��B
�'B
��B
|�B
T�B
J�B
%�B
bB
B
  B	��B	�fB	ƨB	�^B	�9B	��B	��B	�\B	�1B	�B	|�B	p�B	aHB	W
B	R�B	G�B	:^B	2-B	%�B	'�B	0!B	+B	%�B	�B	�B	uB	bB	DB	B��B��B��B�B�B�B�B�sB�TB�BB�#B��B��BǮBB�qB�FB�-B�B�B��B��B��B��B�hB�DB�+B�B~�Bz�By�Bw�Bv�Bu�Bs�Bp�BiyBhsBgmBgmBhsBcTBbNB`BB_;B^5B]/B[#BYBVBQ�BP�BM�BL�BG�BF�BD�BB�B?}B@�B=qB=qB;dB9XB7LB5?B2-B2-B0!B.B.B-B-B-B,B,B,B+B+B+B)�B'�B&�B$�B$�B#�B$�B#�B$�B$�B$�B%�B$�B$�B$�B$�B%�B$�B$�B%�B#�B$�B#�B%�B$�B&�B'�B'�B)�B&�B%�B&�B'�B&�B)�B)�B)�B)�B+B,B+B+B+B-B-B.B1'B2-B33B5?B8RB7LB8RB;dB<jBE�BG�BG�BK�BL�BM�BN�BO�BQ�BW
B[#B\)B^5B`BBe`BhsBiyBk�Bm�Bn�Bm�Bq�Bs�Bt�Bv�Bv�Bv�Bw�Bz�B{�B|�B�B�7B�=B�DB�JB�PB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�?B�RB�jB��BƨB��B�#B�TB�yB�B�B��B��B��B��B	B	
=B	hB	uB	�B	�B	�B	�B	#�B	#�B	&�B	&�B	)�B	)�B	-B	1'B	7LB	;dB	<jB	>wB	A�B	B�B	B�B	D�B	F�B	J�B	K�B	Q�B	R�B	VB	XB	W
B	ZB	[#B	\)B	^5B	^5B	]/B	]/B	cTB	k�B	q�B	q�B	q�B	o�B	p�B	q�B	q�B	r�B	t�B	v�B	x�B	y�B	}�B	�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�\B	�\B	�hB	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�?B	�9B	�?B	�9B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�RB	�dB	�wB	��B	��B	B	ÖB	ĜB	ǮB	ȴB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
VB
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
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
+B
+B
+B
,B
,B
,B
,B
-B
-B
.B
1'B
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
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
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
<jB
<jB
<jB
=qB
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
C�B
B�B
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
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
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
Q�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
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
\)B
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
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
m�B
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
q�B
q�B
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B'�B'�B&�B&�B&�B&�B&�B)�B(�B(�B)�B*�B*�B*�B)�B(�B'�B&�B#�BNB�B �B �B	B@B$�B+�B/�B]BncB�B�TB��B��B��B��B� B� B��B��B�B�$B�B�B�B�2B�B�B��B��B� B��B��B�B�B��B��B��B��B��B�qB�SB�FB�(B��B�B~�Bw�Bg8BbB^BT�BH�B@OB@OB=<B88B"�B�B�qB�2B�!B��B� B��BɆBǔB�B�MBr|BP�B4B.B
��B
��B
��B
��B
�]B
��B
ȀB
ʌB
�oB
��B
�~B
|�B
T�B
J�B
%�B
.B
�B	��B	��B	�2B	ƎB	�*B	�B	��B	�MB	�(B	��B	��B	|�B	poB	aB	V�B	R�B	GzB	:*B	1�B	%�B	'�B	/�B	*�B	%�B	xB	SB	@B	.B	B	�B��B��B��B�|B�iB�cB�]B�>B� B�B��BөBΥB�zB�[B�<B��B��B��B��B��B��B�qB�YB�4B�B��B��B~�Bz�By�Bw�BvzBu�BshBpoBiDBh>Bg8Bg8Bh>Bc BbB`B_B^B\�BZ�BX�BU�BQ�BP�BM�BL~BG_BFtBDgBB[B?HB@OB="B=<B;0B9	B6�B4�B1�B1�B/�B-�B-�B,�B,�B,�B+�B+�B+�B*�B*�B*�B)�B'�B&�B$�B$�B#�B$�B#�B$�B$�B$�B%�B$�B$�B$�B$�B%�B$�B$�B%�B#�B$�B#�B%�B$�B&�B'�B'�B)�B&�B%�B&�B'�B&�B)�B)�B)�B)�B*�B+�B*�B*�B*�B,�B,�B-�B0�B1�B2�B5B8B7B8B;B<6BEmBG_BGzBK�BL�BM�BN�BO�BQ�BV�BZ�B[�B]�B_�Be,Bh>BiDBkQBmCBncBmCBqvBs�Bt�Bv�BvzBvzBw�Bz�B{�B|�B��B��B��B��B�B�B�&B�,B�?B�]B�]B�~B��B��B��B��B��B��B��B�B�B�6B�;B�tBѷB��B�B�DB�]B�[B��B��B��B��B	�B		�B	B	@B	2B	SB	QB	xB	#�B	#�B	&�B	&�B	)�B	)�B	,�B	0�B	7B	;0B	<B	>(B	A;B	B[B	B[B	DgB	FYB	JrB	KxB	Q�B	R�B	U�B	W�B	V�B	Y�B	Z�B	[�B	^B	^B	\�B	\�B	cB	kQB	qvB	qvB	qvB	oOB	pUB	q[B	q[B	r|B	t�B	vzB	x�B	y�B	}�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�4B	�FB	�@B	�@B	�SB	�eB	�kB	�WB	�jB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�(B	�4B	�;B	�AB	�aB	�MB	�zB	ȀB	�_B	�fB	�lB	ʌB	�xB	�xB	�xB	�~B	̈́B	ΥB	ϫB	ѷB	ңB	ԯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	� B	�&B	�,B	�B	�2B	�B	�DB	�*B	�KB	�KB	�WB	�=B	�CB	�cB	�OB	�OB	�OB	�oB	�oB	�oB	�oB	�oB	�vB	�aB	�hB	�hB	�B	�B	�B	�B	�B	�B	�nB	�B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

	B

	B
B

�B
B

�B

�B
B
B
B
B
B
B
B
"B
(B
(B
.B
B
4B
B
4B
4B
4B
4B
4B
B
:B
&B
&B
&B
&B
@B
FB
,B
,B
,B
,B
FB
MB
SB
SB
9B
?B
YB
YB
_B
EB
_B
KB
kB
kB
QB
kB
QB
QB
kB
qB
WB
xB
xB
]B
~B
~B
dB
�B
�B
�B
�B
�B
�B
�B
pB
pB
�B
 �B
 �B
 vB
!�B
!|B
!�B
!|B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
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
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
4B
3�B
4B
4�B
5B
5B
4�B
6B
5�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9$B
9$B
9$B
9$B
9$B
9$B
9$B
:*B
:*B
:B
:*B
;B
<B
<6B
<6B
<B
="B
>BB
?HB
?HB
?HB
?.B
?HB
?.B
?.B
@4B
@4B
A;B
A;B
A;B
A;B
AUB
B[B
BAB
B[B
B[B
B[B
CGB
BAB
CGB
CaB
CaB
CaB
CGB
DgB
DMB
DMB
DMB
DgB
EmB
EmB
EmB
FtB
FtB
FtB
G_B
GzB
GzB
GzB
H�B
H�B
H�B
IlB
IlB
I�B
IlB
I�B
JrB
JrB
J�B
J�B
JrB
JrB
J�B
JrB
J�B
JrB
JrB
K�B
K�B
K�B
KxB
L~B
L�B
L�B
L~B
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
Q�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
U�B
U�B
U�B
U�B
V�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
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
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
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
[�B
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
^B
]�B
^�B
^�B
_B
^�B
_B
^�B
`B
_�B
`B
_�B
`B
`�B
aB
aB
`�B
`�B
aB
aB
aB
a�B
a�B
bB
bB
bB
c B
c B
c B
cB
c B
d&B
d&B
d&B
d&B
dB
d&B
d&B
d&B
eB
e,B
eB
eB
fB
fB
fB
g8B
g8B
g8B
g8B
g8B
gB
h>B
h$B
h$B
h$B
h$B
h$B
i*B
iDB
iDB
iDB
jKB
jKB
j0B
jKB
jKB
kQB
k6B
kQB
kQB
kQB
k6B
kQB
k6B
lWB
l=B
lWB
lWB
m]B
m]B
m]B
m]B
ncB
ncB
nIB
ncB
oOB
oiB
oiB
oiB
oiB
oOB
oOB
oOB
oiB
pUB
poB
pUB
poB
pUB
q[B
qvB
qvB
qvB
q[B
q[B
q[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.49(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810110034382018101100343820181011003438201810120026312018101200263120181012002631JA  ARFMdecpA19c                                                                20181006153514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181006063515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181006063516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181006063516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181006063517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181006063517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181006063517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181006063517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181006063517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181006063518                      G�O�G�O�G�O�                JA  ARUP                                                                        20181006065853                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181006154612  CV  JULD            G�O�G�O�F�62                JM  ARGQJMQC2.0                                                                 20181006154612  CV  JULD_LOCATION   G�O�G�O�F�6@                JM  ARGQJMQC2.0                                                                 20181006154612  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20181006154612  CV  LONGITUDE       G�O�G�O���                JM  ARCAJMQC2.0                                                                 20181010153438  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181010153438  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181011152631  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                