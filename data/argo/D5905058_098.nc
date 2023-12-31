CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-23T09:35:27Z creation;2018-10-23T09:35:30Z conversion to V3.1;2019-12-23T06:13:12Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181023093527  20200120021524  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA  I2_0675_098                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؋
W:� 1   @؋	{B�@6���f�B�cYo���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʃ3D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��H@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ǮC }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3%�D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE��DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD�{D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�DÏ�D�ϮD��D�O�Dď�D�ϮD��D�L{Dŏ�D�ϮD��D�O�DƏ�D�ϮD��D�O�DǏ�D�ϮD��D�O�Dȏ�D�ϮD��D�O�Dɏ�D�ϮD��D�O�Dʒ�D�ϮD��D�O�Dˏ�D�ϮD��D�O�D̏�D�ϮD��D�O�D͏�D�ϮD��D�O�DΏ�D�ϮD��D�O�DϏ�D�ϮD��D�O�DЏ�D�ϮD��D�O�Dя�D�ϮD��D�O�Dҏ�D�ϮD��D�O�Dӏ�D�ϮD��D�O�Dԏ�D�ϮD��D�O�DՏ�D�ϮD��D�O�D֏�D�ϮD��D�O�D׏�D�ϮD��D�O�D؏�D�ϮD��D�O�Dُ�D�ϮD��D�O�Dڏ�D�ϮD��D�O�Dۏ�D�ϮD��D�O�D܏�D�ϮD�{D�L{Dݏ�D�ϮD��D�O�Dޏ�D�ϮD��D�O�Dߏ�D�ϮD��D�O�D���D�ϮD��D�O�DᏮD�ϮD��D�O�D⏮D�ϮD��D�O�D㏮D�ϮD��D�O�D䏮D�ϮD��D�O�D叮D�ϮD��D�O�D揮D�ϮD��D�O�D珮D�ϮD��D�O�D菮D�ϮD��D�O�D鏮D�ϮD��D�O�DꏮD�ϮD��D�O�D돮D�ϮD��D�O�D쏮D�ϮD��D�O�D폮D�ϮD��D�O�DD�ϮD��D�O�DD�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD�{D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�=qA�7LA�/A��A� �A��;AЗ�A�p�A�-A� �A��A��A�-A�-A�9XA�9XA�;dA�G�A�G�A�C�A�E�A�A�A�A�A�9XA�-A��A���A�z�A� �AǺ^Aƺ^Aç�A�|�A���A�{A�+A�A�I�A��jA���A�A�A��
A�K�A��#A��FA�-A�l�A�~�A��mA�jA��`A���A�hsA�/A�bNA���A�ffA���A�=qA���A�5?A���A��7A�/A���A��#A�9XA���A��A�-A�ȴA��A��wA�G�A�p�A��A�oA��-A�9XA��A���A���A�r�A�1A��yA�ƨA���A��\A�n�A���A�JA�C�A���A���A�/A���A��#A�(�A��A���A��FA�bNA��+A��HA�jA�Ap�Az��Ay�Ax�AvZAs�TAr�\Ap��An�RAl �Ah^5Ad�uAb^5AaK�A_&�A\r�AX��AX{AW��AWG�AU|�ASS�APĜAN~�AM7LALE�AJbAI�7AH�yAF�HADI�ABffAA��A@�/AAp�A@�+A?�wA>-A<=qA:ȴA8r�A7�A7VA7
=A6�/A61'A5�-A5�A5XA4I�A2-A1A0�DA/�FA.�A-"�A,��A+��A+"�A*�A*5?A)�^A'�TA&M�A%33A$�DA#�
A#��A!x�A�A&�AM�A�mA��A�mA�-AVAȴA�uAQ�A�
A��AAS�A
=A�A��A?}A+AjA/AVAA�A��A�`AJA`BAȴAK�A	�wA��A��AbNA��A��A�A&�AjA/A V@�S�@���@�Q�@�ff@�  @���@���@��@�Z@�F@�@��@�/@�@�dZ@�-@�/@��@�\)@��@�V@��
@�@�"�@�-@�@�(�@�+@�E�@�@�Ĝ@���@�|�@���@ݺ^@��;@��H@�`B@�  @��@�x�@ԣ�@�r�@�t�@�J@�t�@�/@�S�@�`B@���@���@���@��@�v�@ź^@�1@�l�@���@�=q@��@��j@���@�^5@�&�@��j@�b@�o@�-@�`B@��@��;@�~�@�$�@�p�@��j@��@��R@�=q@��^@���@��
@�@�^5@��@��j@�b@�C�@�ȴ@���@���@���@�I�@�(�@�b@��P@���@�n�@�^5@�M�@���@���@��h@���@��D@� �@�  @��m@��
@���@�S�@��@��@��!@�v�@�~�@�-@��@�V@�j@���@��@� �@�1'@�  @�1@�b@��@�1@��
@�ƨ@���@���@�|�@�C�@�
=@��!@�v�@��@�@���@���@�J@���@���@��@��@��@�@��\@�-@��@�@�p�@���@�z�@�b@��;@��w@��P@�l�@�dZ@�S�@�+@��@���@�ȴ@���@���@�V@��@��@���@��-@�`B@�V@�r�@���@�\)@��@�ȴ@��R@���@�M�@�$�@���@���@�p�@��@���@���@��u@�I�@��@��@�t�@�\)@�K�@�o@��@��R@�~�@�ff@�E�@��@��h@�`B@�7L@��@�%@��9@�bN@���@��F@���@���@���@�;d@�;d@�n�@�{@�{@�{@�J@���@���@��7@�O�@�V@�Ĝ@��9@���@��u@��@�Q�@��m@��@�|�@�@���@��!@���@�v�@�E�@���@���@���@���@�x�@�hs@��@�Z@�  @���@�+@���@��H@���@���@�ff@�{@��^@��7@��@�x�@�G�@��@���@��9@��D@��@�Q�@�b@�@��@+@~�+@}@}/@|�/@{��@z��@z�@y��@y�#@y�^@y��@yx�@x�u@x  @w��@wl�@v�y@v��@v��@v�+@v5?@v@u�-@u/@t��@t�@t��@tI�@sƨ@sdZ@r�@r��@r-@q�#@q��@q��@q�^@q%@p��@p�@pbN@pA�@o�@o�@o�P@ol�@oK�@o�@n��@nȴ@nv�@nE�@n$�@n@m�h@mO�@l�@l�j@l��@lZ@l1@k�F@ko@j��@j��@j~�@j~�@j^5@i��@i��@i�7@i��@ix�@i7L@i�@hb@gK�@f�y@f�y@fȴ@fv�@fff@f{@e�-@e��@e��@e��@ep�@e`B@eO�@d�/@c�F@b��@b~�@a�@ahs@`��@`��@_��@^�R@^��@^ff@^ff@^E�@^$�@^{@^@]��@]�@]?}@]�@]V@\�/@\Z@[�m@[ƨ@[��@[��@[t�@[S�@["�@Z�H@Z��@Z��@Z�!@Z�!@Z�!@Z�\@Z~�@Zn�@Z�@Yx�@X�`@X�9@XQ�@Xb@W�@W�P@W\)@W�@W
=@V��@U�T@U/@T�j@T��@Tj@T1@S�
@Sƨ@S��@S��@SS�@R��@Rn�@Q�@Q�7@Qx�@Qhs@QG�@Q&�@P�`@P�@O�@O��@O;d@Nv�@M��@M��@Mp�@M?}@L��@L�/@L�@Lz�@L(�@K�F@KC�@J��@I��@I�7@IG�@H�9@Hb@Gl�@G+@F��@Fȴ@F��@FV@F{@E�T@E�h@E/@D�@Dj@C��@B��@B=q@A��@A�^@A7L@@��@@A�@?|�@?K�@?+@?
=@?
=@>��@>ff@>5?@=��@=p�@=?}@<��@<z�@<Z@<9X@<1@;�
@;�F@;�@;"�@:��@:n�@:M�@:-@9��@9��@9hs@9%@8�9@8�9@8bN@7��@7+@6ȴ@6��@6v�@6E�@6E�@6E�@6{@5�-@5O�@4��@4�D@49X@3��@3dZ@3"�@3@2��@2�\@2n�@2�@1�#@1��@1x�@1&�@0r�@/�;@/��@/\)@/K�@/+@/+@/�@.��@.��@.5?@.{@-�-@-�@-`B@-/@,�/@,�/@,�j@,�D@,Z@,(�@+��@+�
@+�@+33@+@*�@*�@*�H@*��@*��@*��@*~�@*=q@)�#@)�7@)hs@)&�@(��@(�9@(�u@(�@(bN@(bN@(A�@(A�@(A�@(A�@(1'@'�@'�@'|�@'�@&�y@&��@&��@&�@&�R@&v�@&$�@%p�@%?}@%/@$�@$j@$I�@$�@#�
@#�F@#�F@#�F@#��@#t�@#t�@#t�@#dZ@#33@"�H@"�!@"�\@"n�@"^5@"M�@"-@!��@!�#@!�7@!G�@!%@ �9@  �@�@�w@��@�P@\)@
=@�R@��@V@V@5?@�@@��@�@O�@/@V@��@��@Z@9X@9X@��@�m@�m@ƨ@��@dZ@C�@o@@�@��@~�@��@x�@7L@��@Ĝ@Ĝ@Ĝ@�@��@l�@K�@�@
=@��@�R@ff@{@��@�-@��@�@?}@�/@�@z�@Z@I�@9X@�@��@�m@�
@��@"�@@��@~�@n�@n�@^5@^5@^5@M�@�@7L@�`@�u@bN@ �@�@�w@��@|�@K�@;d@+@�@��@�R@�+@v�@ff@5?@$�@{@�T@�T@�T@�-@�h@O�@�@�/@��@��@�D@j@Z@9X@9X@�@1@��@�m@ƨ@�F@dZ@o@
�H@
��@
n�@
=q@	��@	�#@	��@	G�@	&�@��@�`@Ĝ@�9@��@�@r�@Q�@A�@ �@  @�@��@�P@|�@\)@�@��@�@�+@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�;dA�=qA�7LA�/A��A� �A��;AЗ�A�p�A�-A� �A��A��A�-A�-A�9XA�9XA�;dA�G�A�G�A�C�A�E�A�A�A�A�A�9XA�-A��A���A�z�A� �AǺ^Aƺ^Aç�A�|�A���A�{A�+A�A�I�A��jA���A�A�A��
A�K�A��#A��FA�-A�l�A�~�A��mA�jA��`A���A�hsA�/A�bNA���A�ffA���A�=qA���A�5?A���A��7A�/A���A��#A�9XA���A��A�-A�ȴA��A��wA�G�A�p�A��A�oA��-A�9XA��A���A���A�r�A�1A��yA�ƨA���A��\A�n�A���A�JA�C�A���A���A�/A���A��#A�(�A��A���A��FA�bNA��+A��HA�jA�Ap�Az��Ay�Ax�AvZAs�TAr�\Ap��An�RAl �Ah^5Ad�uAb^5AaK�A_&�A\r�AX��AX{AW��AWG�AU|�ASS�APĜAN~�AM7LALE�AJbAI�7AH�yAF�HADI�ABffAA��A@�/AAp�A@�+A?�wA>-A<=qA:ȴA8r�A7�A7VA7
=A6�/A61'A5�-A5�A5XA4I�A2-A1A0�DA/�FA.�A-"�A,��A+��A+"�A*�A*5?A)�^A'�TA&M�A%33A$�DA#�
A#��A!x�A�A&�AM�A�mA��A�mA�-AVAȴA�uAQ�A�
A��AAS�A
=A�A��A?}A+AjA/AVAA�A��A�`AJA`BAȴAK�A	�wA��A��AbNA��A��A�A&�AjA/A V@�S�@���@�Q�@�ff@�  @���@���@��@�Z@�F@�@��@�/@�@�dZ@�-@�/@��@�\)@��@�V@��
@�@�"�@�-@�@�(�@�+@�E�@�@�Ĝ@���@�|�@���@ݺ^@��;@��H@�`B@�  @��@�x�@ԣ�@�r�@�t�@�J@�t�@�/@�S�@�`B@���@���@���@��@�v�@ź^@�1@�l�@���@�=q@��@��j@���@�^5@�&�@��j@�b@�o@�-@�`B@��@��;@�~�@�$�@�p�@��j@��@��R@�=q@��^@���@��
@�@�^5@��@��j@�b@�C�@�ȴ@���@���@���@�I�@�(�@�b@��P@���@�n�@�^5@�M�@���@���@��h@���@��D@� �@�  @��m@��
@���@�S�@��@��@��!@�v�@�~�@�-@��@�V@�j@���@��@� �@�1'@�  @�1@�b@��@�1@��
@�ƨ@���@���@�|�@�C�@�
=@��!@�v�@��@�@���@���@�J@���@���@��@��@��@�@��\@�-@��@�@�p�@���@�z�@�b@��;@��w@��P@�l�@�dZ@�S�@�+@��@���@�ȴ@���@���@�V@��@��@���@��-@�`B@�V@�r�@���@�\)@��@�ȴ@��R@���@�M�@�$�@���@���@�p�@��@���@���@��u@�I�@��@��@�t�@�\)@�K�@�o@��@��R@�~�@�ff@�E�@��@��h@�`B@�7L@��@�%@��9@�bN@���@��F@���@���@���@�;d@�;d@�n�@�{@�{@�{@�J@���@���@��7@�O�@�V@�Ĝ@��9@���@��u@��@�Q�@��m@��@�|�@�@���@��!@���@�v�@�E�@���@���@���@���@�x�@�hs@��@�Z@�  @���@�+@���@��H@���@���@�ff@�{@��^@��7@��@�x�@�G�@��@���@��9@��D@��@�Q�@�b@�@��@+@~�+@}@}/@|�/@{��@z��@z�@y��@y�#@y�^@y��@yx�@x�u@x  @w��@wl�@v�y@v��@v��@v�+@v5?@v@u�-@u/@t��@t�@t��@tI�@sƨ@sdZ@r�@r��@r-@q�#@q��@q��@q�^@q%@p��@p�@pbN@pA�@o�@o�@o�P@ol�@oK�@o�@n��@nȴ@nv�@nE�@n$�@n@m�h@mO�@l�@l�j@l��@lZ@l1@k�F@ko@j��@j��@j~�@j~�@j^5@i��@i��@i�7@i��@ix�@i7L@i�@hb@gK�@f�y@f�y@fȴ@fv�@fff@f{@e�-@e��@e��@e��@ep�@e`B@eO�@d�/@c�F@b��@b~�@a�@ahs@`��@`��@_��@^�R@^��@^ff@^ff@^E�@^$�@^{@^@]��@]�@]?}@]�@]V@\�/@\Z@[�m@[ƨ@[��@[��@[t�@[S�@["�@Z�H@Z��@Z��@Z�!@Z�!@Z�!@Z�\@Z~�@Zn�@Z�@Yx�@X�`@X�9@XQ�@Xb@W�@W�P@W\)@W�@W
=@V��@U�T@U/@T�j@T��@Tj@T1@S�
@Sƨ@S��@S��@SS�@R��@Rn�@Q�@Q�7@Qx�@Qhs@QG�@Q&�@P�`@P�@O�@O��@O;d@Nv�@M��@M��@Mp�@M?}@L��@L�/@L�@Lz�@L(�@K�F@KC�@J��@I��@I�7@IG�@H�9@Hb@Gl�@G+@F��@Fȴ@F��@FV@F{@E�T@E�h@E/@D�@Dj@C��@B��@B=q@A��@A�^@A7L@@��@@A�@?|�@?K�@?+@?
=@?
=@>��@>ff@>5?@=��@=p�@=?}@<��@<z�@<Z@<9X@<1@;�
@;�F@;�@;"�@:��@:n�@:M�@:-@9��@9��@9hs@9%@8�9@8�9@8bN@7��@7+@6ȴ@6��@6v�@6E�@6E�@6E�@6{@5�-@5O�@4��@4�D@49X@3��@3dZ@3"�@3@2��@2�\@2n�@2�@1�#@1��@1x�@1&�@0r�@/�;@/��@/\)@/K�@/+@/+@/�@.��@.��@.5?@.{@-�-@-�@-`B@-/@,�/@,�/@,�j@,�D@,Z@,(�@+��@+�
@+�@+33@+@*�@*�@*�H@*��@*��@*��@*~�@*=q@)�#@)�7@)hs@)&�@(��@(�9@(�u@(�@(bN@(bN@(A�@(A�@(A�@(A�@(1'@'�@'�@'|�@'�@&�y@&��@&��@&�@&�R@&v�@&$�@%p�@%?}@%/@$�@$j@$I�@$�@#�
@#�F@#�F@#�F@#��@#t�@#t�@#t�@#dZ@#33@"�H@"�!@"�\@"n�@"^5@"M�@"-@!��@!�#@!�7@!G�@!%@ �9@  �@�@�w@��@�P@\)@
=@�R@��@V@V@5?@�@@��@�@O�@/@V@��@��@Z@9X@9X@��@�m@�m@ƨ@��@dZ@C�@o@@�@��@~�@��@x�@7L@��@Ĝ@Ĝ@Ĝ@�@��@l�@K�@�@
=@��@�R@ff@{@��@�-@��@�@?}@�/@�@z�@Z@I�@9X@�@��@�m@�
@��@"�@@��@~�@n�@n�@^5@^5@^5@M�@�@7L@�`@�u@bN@ �@�@�w@��@|�@K�@;d@+@�@��@�R@�+@v�@ff@5?@$�@{@�T@�T@�T@�-@�h@O�@�@�/@��@��@�D@j@Z@9X@9X@�@1@��@�m@ƨ@�F@dZ@o@
�H@
��@
n�@
=q@	��@	�#@	��@	G�@	&�@��@�`@Ĝ@�9@��@�@r�@Q�@A�@ �@  @�@��@�P@|�@\)@�@��@�@�+@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bm�Bm�Bm�Bl�BjBjBffB`BB\)BXBXBXBYB]/B^5BaHBbNBdZBgmBhsBiyBiyBjBk�Bk�BjBiyBdZBaHBu�B�1B�=B��B��B��B��B��B��B��B��B��B��B�bB�=B�+B�1B�Bw�B~�Bz�By�Bz�B� B~�B|�B�B� By�Bq�B\)B[#BT�BM�BH�BD�BI�B?}B1'B&�B�B�B�B�B
=B\BoBBB��B��B�B�mB�BǮB��B�B��B��B�{B�hB�%Bm�B^5BO�BE�B7LB+B�B
��B
�B
�B
�fB
�B
B
�?B
��B
��B
�oB
t�B
Q�B
B�B
7LB
%�B
\B
B	��B	�ZB	��B	�-B	��B	{�B	x�B	m�B	^5B	6FB	1'B	9XB	9XB	1'B	�B	B�B�HB�5BɺBȴB��B�jB�B��B��B��B�B�FB�'B��B�{B�\B�B{�Bz�B{�B}�B}�B�B�7B�1B�Bt�Bu�Br�Bo�Bn�Bk�BiyBgmBo�Bv�Bv�Bw�Bv�Bn�BhsBffBe`BjB\)BO�BP�BVBW
B[#B^5B`BB`BB^5B]/B]/B[#BZBYBVBT�BE�BC�B@�B@�BA�B>wB<jB<jB>wB6FB8RB8RB6FB5?B0!B.B,B+B'�B&�B$�B#�B#�B"�B�B �B �B�B �B �B�B�B�B�B�B�B�B�B�B�B �B!�B �B �B!�B&�B'�B'�B'�B)�B(�B'�B'�B(�B(�B(�B(�B)�B,B+B/B-B.B.B.B/B-B-B.B.B33B49B7LB;dB;dB;dB=qB?}B?}B?}BH�BI�BI�BK�BR�BXB[#B]/B]/B^5B_;B`BB`BB`BB`BBffBiyBiyBjBjBl�Br�Bs�Bv�B}�B� B�B�%B�DB�VB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�FB�dB�}BÖBǮBɺB��B��B��B�B�#B�;B�NB�`B�B�B��B��B��B��B	B	%B	%B	+B	
=B	VB	oB	{B	{B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	$�B	(�B	,B	2-B	7LB	;dB	=qB	>wB	>wB	B�B	G�B	J�B	K�B	M�B	N�B	Q�B	Q�B	VB	YB	\)B	`BB	cTB	dZB	dZB	e`B	hsB	iyB	iyB	jB	k�B	m�B	m�B	n�B	o�B	s�B	u�B	x�B	{�B	|�B	~�B	� B	�B	�B	�B	�7B	�7B	�DB	�DB	�JB	�PB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�XB	�^B	�dB	�jB	�jB	�qB	��B	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�5B	�5B	�5B	�5B	�;B	�5B	�BB	�NB	�TB	�`B	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
%�B
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
+B
,B
,B
-B
-B
-B
-B
-B
.B
-B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
2-B
2-B
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
6FB
6FB
6FB
6FB
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
@�B
?}B
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
F�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
H�B
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
K�B
K�B
K�B
K�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
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
YB
YB
YB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
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
ffB
ffB
ffB
ffB
e`B
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
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bm]Bm]Bm]BlqBjeBjeBf2B`'B[�BW�BW�BW�BX�B\�B^Ba-Bb4Bd&Bg8BhXBi_Bi_BjKBkQBkkBjKBiDBd&BaBu�B�B�	B�_B�_B�xB��B��B��B��B�xB�eB�MB�HB�	B��B��B��Bw�B~�Bz�By�Bz�B�B~�B|�B��B�By�BqvB[�B[	BT�BM�BH�BDgBI�B?HB0�B&�B~BeBYB_B
	B(B:B�B �B��B�B�oB�8B��B�zB�UB��B�eB�YB�FB�4B��Bm]B^BO�BEmB72B*�BYB
��B
�iB
�QB
�2B
��B
�[B
�B
��B
��B
�:B
t�B
Q�B
B[B
7B
%�B
(B
�B	��B	�&B	бB	��B	�MB	{�B	x�B	m]B	^B	6B	0�B	9$B	9$B	0�B	�B	�B�]B�B�BɆBȀBʌB�6B��B�xB�_B�YB��B��B��B��B�FB�(B��B{�Bz�B{�B}�B}�B��B�B��B��Bt�Bu�Br|BoiBncBkQBiDBg8BoOBv�Bv�Bw�Bv�BncBh>BfBeBjKB[�BO�BP�BU�BV�BZ�B^B`B`B^B\�B\�BZ�BY�BX�BU�BT�BESBCaB@OB@OBAUB>BB<6B<B>BB6B8B8B6B5B/�B-�B+�B*�B'�B&�B$�B#�B#�B"�B�B vB �B�B vB vBpB�BjBjB�B�B�B�BjB�B vB!|B �B �B!�B&�B'�B'�B'�B)�B(�B'�B'�B(�B(�B(�B(�B)�B+�B*�B.�B,�B-�B-�B-�B.�B,�B,�B-�B-�B2�B4B7B;0B;0B;0B="B?.B?.B?.BH�BI�BI�BKxBR�BW�BZ�B\�B\�B]�B_B`B`B_�B_�BfBi*BiDBjKBj0BlWBraBshBvzB}�B�B��B��B��B�B�B�,B�9B�QB�dB��B�vB�|B�|B��B��B��B��B��B��B�B�B�0B�HB�aB�zBɆB�rB͟BѷB��B��B��B�B�,B�WB�|B�zB��B��B��B	�B	�B	�B	�B		�B	"B	:B	,B	,B	MB	qB	jB	�B	 �B	"�B	"�B	"�B	$�B	(�B	+�B	1�B	6�B	;0B	=<B	>BB	>BB	BAB	GzB	J�B	KxB	M�B	N�B	Q�B	Q�B	U�B	X�B	[�B	_�B	cB	d&B	d&B	eB	h>B	iDB	i*B	jKB	k6B	m]B	mCB	ncB	oiB	shB	u�B	x�B	{�B	|�B	~�B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�(B	�B	�:B	�MB	�YB	�_B	�_B	�_B	�WB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�*B	�0B	�B	�B	�<B	�OB	�mB	�zB	�fB	ȀB	�fB	�fB	ʌB	�xB	�~B	̈́B	ϑB	ϑB	ϑB	ϫB	ЗB	ѝB	ңB	өB	��B	ּB	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�8B	�8B	�>B	�$B	�DB	�0B	�QB	�6B	�WB	�=B	�WB	�]B	�cB	�IB	�iB	�iB	�oB	�vB	�vB	�|B	�aB	�hB	�B	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
	�B

	B
B
�B
B
B
B
B
B
"B
"B
(B
B
(B
B
B
.B
4B
B
 B
 B
:B
 B
:B
 B
:B
 B
:B
:B
:B
 B
&B
FB
2B
SB
?B
?B
_B
_B
KB
QB
QB
QB
WB
qB
qB
WB
WB
WB
xB
]B
xB
xB
xB
~B
~B
~B
�B
dB
jB
jB
jB
jB
jB
jB
jB
jB
jB
pB
pB
jB
pB
 �B
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
%�B
%�B
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
*�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-�B
,�B
-�B
-�B
.�B
.�B
/�B
/�B
0�B
0�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
5B
5B
4�B
6B
6B
5�B
5�B
7B
6�B
6�B
8B
9$B
9$B
9$B
9$B
9$B
:*B
:B
:B
;B
;0B
<B
<B
<6B
<6B
=<B
=<B
=<B
=<B
=<B
>BB
>(B
>BB
>BB
>(B
>(B
?.B
?HB
?.B
@4B
?.B
@OB
@4B
A;B
A;B
AUB
AUB
AUB
A;B
AUB
AUB
A;B
BAB
B[B
CGB
CaB
CaB
DgB
DgB
DgB
DgB
DMB
DgB
DMB
EmB
EmB
ESB
EmB
FtB
FYB
FtB
FtB
FYB
FYB
GzB
FYB
G_B
GzB
GzB
G_B
HfB
H�B
HfB
HfB
IlB
IlB
IlB
IlB
IlB
JrB
J�B
JrB
J�B
K�B
K�B
KxB
KxB
K�B
KxB
K�B
K�B
KxB
K�B
L�B
L~B
L~B
L~B
M�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
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
X�B
X�B
X�B
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
^B
]�B
^B
^B
]�B
]�B
]�B
_B
`B
`B
`B
`B
_�B
aB
`�B
aB
a�B
bB
a�B
a�B
a�B
cB
cB
cB
c B
d&B
d&B
d&B
dB
d&B
eB
eB
e,B
eB
e,B
e,B
eB
f2B
f2B
fB
fB
eB
fB
f2B
fB
g8B
g8B
gB
gB
gB
h>B
h>B
h>B
h>B
h$B
h>B
h$B
iDB
iDB
i*B
iDB
i*B
iDB
i*B
i*B
i*B
i*B
i*B
j0B
j0B
j0B
jKB
jKB
jKB
j0B
kQB
k6B
kQB
k6B
kQB
k6B
k6B
kQB
k6B
kQB
k6B
kQB
lWB
l=B
l=B
l=B
mCB
mCB
mCB
nIB
ncB
ncB
ncB
ncB
ncB
oiB
oOB
oOB
oiB
oOB
oiB
oiB
pUB
poB
poB
q[B
q[B
raB
raB
r|B
r|B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.49(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810280040162018102800401620181028004016201810290039142018102900391420181029003914JA  ARFMdecpA19c                                                                20181023183516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181023093527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181023093528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181023093528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181023093529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181023093529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181023093529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181023093529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181023093530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181023093530                      G�O�G�O�G�O�                JA  ARUP                                                                        20181023095556                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181023154054  CV  JULD            G�O�G�O�F�XS                JM  ARGQJMQC2.0                                                                 20181023154054  CV  JULD_LOCATION   G�O�G�O�F�X`                JM  ARGQJMQC2.0                                                                 20181023154054  CV  LONGITUDE       G�O�G�O����                JM  ARCAJMQC2.0                                                                 20181027154016  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181027154016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181028153914  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021524                      G�O�G�O�G�O�                