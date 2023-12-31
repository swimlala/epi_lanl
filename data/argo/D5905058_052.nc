CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-09T18:35:46Z creation;2018-04-09T18:35:48Z conversion to V3.1;2019-12-23T06:23:52Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܼ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180409183546  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               4A   JA  I2_0675_052                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Y籮ր1   @�Y�`��@6�V��b��c�	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D��3D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@?\)@��H@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCLc�CN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC%�DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]��D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�R�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�DÏ�D�ϮD��D�O�Dď�D���D��D�O�Dŏ�D�ϮD��D�O�DƏ�D�ϮD��D�O�DǏ�D�ϮD��D�O�Dȏ�D�ϮD��D�O�Dɏ�D�ϮD��D�O�Dʏ�D�ϮD��D�O�Dˏ�D�ϮD��D�O�D̏�D�ϮD��D�O�D͏�D�ϮD��D�O�DΏ�D�ϮD��D�O�DϏ�D�ϮD��D�O�DЏ�D�ϮD��D�O�Dя�D�ϮD��D�O�Dҏ�D�ϮD��D�O�Dӏ�D�ϮD��D�O�Dԏ�D�ϮD��D�O�DՏ�D�ϮD��D�O�D֏�D�ϮD��D�O�D׏�D�ϮD��D�O�D؏�D�ϮD��D�O�Dُ�D�ϮD��D�O�Dڏ�D�ϮD��D�O�Dۏ�D�ϮD��D�O�D܏�D�ϮD��D�O�Dݏ�D�ϮD��D�O�Dޏ�D�ϮD��D�O�Dߏ�D�ϮD��D�O�D���D�ϮD��D�O�DᏮD�ϮD��D�O�D⏮D�ϮD��D�O�D㏮D�ϮD��D�O�D䏮D�ϮD��D�O�D叮D�ϮD��D�O�D揮D�ϮD��D�O�D珮D�ϮD��D�O�D菮D�ϮD��D�O�D鏮D�ϮD��D�O�DꏮD�ϮD��D�O�D돮D�ϮD��D�O�D쏮D�ϮD��D�O�D폮D�ϮD��D�O�DD�ϮD��D�O�DD�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D���D�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�JA�
=A��HA�v�A��A���A��A��A�`BA�oA���A��mA�JA�r�A�1'A�1A�z�A���A��`A���A�;dA�{A��TA��!A��uA�x�A�G�A�+A��;A�x�A�XA�?}A�1A��HA��RA���A�\)A�+A� �A�A���A��DA�+A��TA���A�C�A���A�;dA�%A��#A��wA���A���A���A���A��DA���A��-A�1'A��FA�I�A���A�XA�JA��A�^5A�JA��!A�v�A�%A�C�A���A�  A��#A���A�p�A���A��TA���A��`A�ƨA��A��;A�+A���A��TA���A�"�A���A��A�`BA��`A�$�A�33A�Q�A��RA��A|�+A{x�A{oAz~�Az$�Av��AtȴAt�As�Aq�
Ap~�Ao�Aox�Am�FAkƨAh��Ae��Ac��Ab�\Aal�Aa�A`�A]�A[
=AX�\AUC�ASp�ARĜAPQ�AM�^AJVAIK�AGK�AEC�AD9XAC%AAp�A>�`A=�A<z�A;�#A9��A8�uA6�+A5�;A5O�A4�A1"�A/�hA.��A.=qA-�FA,�yA, �A+�A+%A)7LA'�wA'VA&A�A$��A"��A!�-A!hsA �A��AdZAVAl�A/A|�A�\A�A�7A
=A�+Al�AA�A�9A9XA$�A�#AG�A�wAĜA�DAA�AJA�A��A��A�`A�A
��A
1'A	�-A�`AJAG�A��A�`A��A`BA-A��A�A �@��P@���@���@�ƨ@���@��@�b@�5?@�hs@�z�@�^5@@��@���@�`B@�ƨ@�-@��@�X@�j@�A�@�ƨ@�C�@�!@�v�@�^@ߍP@�{@�  @�33@ڏ\@�%@�l�@�?}@�Q�@��m@ӕ�@��@�$�@�`B@�O�@Гu@ϥ�@�"�@�~�@̴9@�ff@�?}@���@�r�@Ǖ�@��@�5?@��`@�r�@���@�+@�@�^5@�{@�@��h@�/@�1'@��w@�dZ@�ȴ@���@���@��@��
@�dZ@��H@��+@��@�G�@�bN@�t�@��@�J@�`B@�Ĝ@��w@��@�5?@��#@�p�@�/@��@���@�(�@��\@�@��h@�7L@�%@�X@�/@���@�1@���@�33@��+@��@��#@��#@�@��h@�O�@�O�@�G�@�G�@�O�@�&�@��`@��;@���@���@�v�@�$�@��@���@���@��@�j@��F@�K�@�K�@�o@��H@��!@���@�n�@�=q@���@�@���@�O�@��@��j@�I�@� �@��F@�t�@���@�J@��7@�?}@�%@�Z@�I�@�b@�t�@���@�M�@��@��@���@�O�@���@��9@���@��@�Z@�A�@�1@��;@��w@���@�t�@�C�@�
=@��+@�E�@��@��@���@���@�hs@�7L@��@��`@��j@�bN@�1'@�b@�  @���@��@��
@��@�t�@�K�@�33@�"�@��R@�E�@�J@���@�hs@�G�@�7L@�V@���@�r�@�Q�@�1'@� �@��@�b@�  @��m@���@��@�\)@�S�@�C�@�o@�ȴ@�v�@�M�@��@��^@�&�@��j@��D@�j@�(�@��w@�l�@��@��@���@�v�@�ff@�J@���@�hs@�X@��@���@��@�j@�Q�@�(�@��m@��@�|�@�C�@�@���@��!@��+@�n�@�V@�E�@�@��#@���@�X@�7L@�/@��@���@��/@��@�I�@�A�@��@�1@��m@�|�@�\)@�+@�"�@��@��y@�ȴ@��R@���@�~�@�V@�J@�@��-@���@��@�&�@���@��@��@��D@�z�@� �@��@l�@;d@+@+@+@�@~�y@~�R@~5?@}�h@}�@|(�@{��@{��@z�@z�!@z��@z-@yx�@xbN@w��@w�P@w|�@wl�@v��@v��@vff@v5?@u��@u�@up�@uO�@u/@u�@uV@t��@t��@t�/@t��@t��@tZ@s��@s�m@s�@r��@rM�@rJ@q�@q�#@q��@q�^@q��@q�@p�`@pr�@pQ�@p �@pb@o�w@n�@n��@nv�@n$�@m�@m?}@l��@l�@k�
@k�@kS�@ko@j�H@j�@j��@i��@iG�@h��@h�@hQ�@hA�@h �@g�@g��@g��@gK�@f��@f$�@e�-@e/@dZ@c��@c��@cdZ@c@b��@b-@a��@a��@ax�@aX@a7L@`��@`r�@_�;@_�w@_��@^�y@^E�@^{@^@]�T@]�h@]p�@]/@]V@\��@\Z@\I�@\9X@[�m@[S�@[o@Z��@Z^5@Z�@Y�@Y��@Yx�@YG�@Y&�@Y%@X�9@X�u@Xr�@X1'@X1'@Xb@Wl�@W;d@V�y@Vff@U�@U��@U�h@Up�@U`B@UO�@U?}@Tj@S��@St�@S33@S@R�@R��@Rn�@RM�@R�@Q�^@Qx�@Q7L@P��@PbN@Pb@O�@O��@O�P@O\)@O�@N��@N{@M�-@M�@M�@L�@Lz�@Kƨ@KC�@J�H@J�!@J�\@J^5@J-@I�^@I%@HQ�@H �@Hb@G��@G|�@F��@F�R@F$�@F@E�@E��@E`B@EV@Dz�@C��@C��@CC�@C@B��@B=q@A��@A�7@A&�@@��@@b@?�@?;d@>ȴ@>�+@>E�@>5?@>{@=��@=�-@=�h@=V@<z�@<(�@;��@;C�@:��@:�!@:~�@:M�@9��@9��@9X@9G�@9G�@9%@8��@8��@8�`@8��@8Q�@81'@8 �@8b@8  @7�@7l�@7
=@6ȴ@6�R@6��@6��@6ff@6V@6E�@6$�@5��@5?}@4��@4z�@49X@3�
@3��@3t�@3dZ@3S�@3C�@333@3"�@3o@2�!@2~�@2�@1��@1�#@1��@1��@1x�@17L@1%@0��@0Ĝ@0�9@0�u@0bN@01'@/�;@/l�@/K�@/�@.��@.�@.��@.�+@.v�@.ff@.5?@-@-�-@-�-@-�@-�@,��@,�@,�D@,9X@,�@+�m@+�
@+ƨ@+�F@+dZ@+"�@+o@*�H@*��@*�\@*J@)�^@)��@)�7@)G�@)%@(A�@(b@'�;@'��@'�P@'\)@'�@&��@&�y@&��@&ff@&5?@&@%��@%�@%`B@%/@$�@$�j@$�D@$I�@$�@#t�@#o@"�!@"=q@"J@!��@!��@!x�@!�@!%@ �`@ �u@ r�@ A�@ b@�@l�@��@ȴ@��@�+@ff@5?@�@�T@@��@p�@�@�@�D@�D@I�@1@t�@C�@�H@~�@^5@�@��@��@hs@%@��@��@�u@Q�@�@�;@�w@�@�P@|�@l�@l�@+@
=@ȴ@��@��@�+@V@�@��@�@p�@`B@O�@?}@V@�j@�@��@z�@(�@1@��@�
@�F@��@t�@dZ@S�@C�@�@��@��@�!@��@�\@~�@�@�#@�#@��@��@hs@7L@��@�9@��@�u@�@r�@bN@Q�@A�@b@b@  @�w@��@|�@;d@
=@�@ȴ@�R@�+@�+@E�@5?@$�@��@�h@`B@V@��@z�@Z@9X@9X@�@��@�
@ƨ@��@t�@C�@"�@@
��@
~�@
^5@
=q@
�@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�JA�
=A��HA�v�A��A���A��A��A�`BA�oA���A��mA�JA�r�A�1'A�1A�z�A���A��`A���A�;dA�{A��TA��!A��uA�x�A�G�A�+A��;A�x�A�XA�?}A�1A��HA��RA���A�\)A�+A� �A�A���A��DA�+A��TA���A�C�A���A�;dA�%A��#A��wA���A���A���A���A��DA���A��-A�1'A��FA�I�A���A�XA�JA��A�^5A�JA��!A�v�A�%A�C�A���A�  A��#A���A�p�A���A��TA���A��`A�ƨA��A��;A�+A���A��TA���A�"�A���A��A�`BA��`A�$�A�33A�Q�A��RA��A|�+A{x�A{oAz~�Az$�Av��AtȴAt�As�Aq�
Ap~�Ao�Aox�Am�FAkƨAh��Ae��Ac��Ab�\Aal�Aa�A`�A]�A[
=AX�\AUC�ASp�ARĜAPQ�AM�^AJVAIK�AGK�AEC�AD9XAC%AAp�A>�`A=�A<z�A;�#A9��A8�uA6�+A5�;A5O�A4�A1"�A/�hA.��A.=qA-�FA,�yA, �A+�A+%A)7LA'�wA'VA&A�A$��A"��A!�-A!hsA �A��AdZAVAl�A/A|�A�\A�A�7A
=A�+Al�AA�A�9A9XA$�A�#AG�A�wAĜA�DAA�AJA�A��A��A�`A�A
��A
1'A	�-A�`AJAG�A��A�`A��A`BA-A��A�A �@��P@���@���@�ƨ@���@��@�b@�5?@�hs@�z�@�^5@@��@���@�`B@�ƨ@�-@��@�X@�j@�A�@�ƨ@�C�@�!@�v�@�^@ߍP@�{@�  @�33@ڏ\@�%@�l�@�?}@�Q�@��m@ӕ�@��@�$�@�`B@�O�@Гu@ϥ�@�"�@�~�@̴9@�ff@�?}@���@�r�@Ǖ�@��@�5?@��`@�r�@���@�+@�@�^5@�{@�@��h@�/@�1'@��w@�dZ@�ȴ@���@���@��@��
@�dZ@��H@��+@��@�G�@�bN@�t�@��@�J@�`B@�Ĝ@��w@��@�5?@��#@�p�@�/@��@���@�(�@��\@�@��h@�7L@�%@�X@�/@���@�1@���@�33@��+@��@��#@��#@�@��h@�O�@�O�@�G�@�G�@�O�@�&�@��`@��;@���@���@�v�@�$�@��@���@���@��@�j@��F@�K�@�K�@�o@��H@��!@���@�n�@�=q@���@�@���@�O�@��@��j@�I�@� �@��F@�t�@���@�J@��7@�?}@�%@�Z@�I�@�b@�t�@���@�M�@��@��@���@�O�@���@��9@���@��@�Z@�A�@�1@��;@��w@���@�t�@�C�@�
=@��+@�E�@��@��@���@���@�hs@�7L@��@��`@��j@�bN@�1'@�b@�  @���@��@��
@��@�t�@�K�@�33@�"�@��R@�E�@�J@���@�hs@�G�@�7L@�V@���@�r�@�Q�@�1'@� �@��@�b@�  @��m@���@��@�\)@�S�@�C�@�o@�ȴ@�v�@�M�@��@��^@�&�@��j@��D@�j@�(�@��w@�l�@��@��@���@�v�@�ff@�J@���@�hs@�X@��@���@��@�j@�Q�@�(�@��m@��@�|�@�C�@�@���@��!@��+@�n�@�V@�E�@�@��#@���@�X@�7L@�/@��@���@��/@��@�I�@�A�@��@�1@��m@�|�@�\)@�+@�"�@��@��y@�ȴ@��R@���@�~�@�V@�J@�@��-@���@��@�&�@���@��@��@��D@�z�@� �@��@l�@;d@+@+@+@�@~�y@~�R@~5?@}�h@}�@|(�@{��@{��@z�@z�!@z��@z-@yx�@xbN@w��@w�P@w|�@wl�@v��@v��@vff@v5?@u��@u�@up�@uO�@u/@u�@uV@t��@t��@t�/@t��@t��@tZ@s��@s�m@s�@r��@rM�@rJ@q�@q�#@q��@q�^@q��@q�@p�`@pr�@pQ�@p �@pb@o�w@n�@n��@nv�@n$�@m�@m?}@l��@l�@k�
@k�@kS�@ko@j�H@j�@j��@i��@iG�@h��@h�@hQ�@hA�@h �@g�@g��@g��@gK�@f��@f$�@e�-@e/@dZ@c��@c��@cdZ@c@b��@b-@a��@a��@ax�@aX@a7L@`��@`r�@_�;@_�w@_��@^�y@^E�@^{@^@]�T@]�h@]p�@]/@]V@\��@\Z@\I�@\9X@[�m@[S�@[o@Z��@Z^5@Z�@Y�@Y��@Yx�@YG�@Y&�@Y%@X�9@X�u@Xr�@X1'@X1'@Xb@Wl�@W;d@V�y@Vff@U�@U��@U�h@Up�@U`B@UO�@U?}@Tj@S��@St�@S33@S@R�@R��@Rn�@RM�@R�@Q�^@Qx�@Q7L@P��@PbN@Pb@O�@O��@O�P@O\)@O�@N��@N{@M�-@M�@M�@L�@Lz�@Kƨ@KC�@J�H@J�!@J�\@J^5@J-@I�^@I%@HQ�@H �@Hb@G��@G|�@F��@F�R@F$�@F@E�@E��@E`B@EV@Dz�@C��@C��@CC�@C@B��@B=q@A��@A�7@A&�@@��@@b@?�@?;d@>ȴ@>�+@>E�@>5?@>{@=��@=�-@=�h@=V@<z�@<(�@;��@;C�@:��@:�!@:~�@:M�@9��@9��@9X@9G�@9G�@9%@8��@8��@8�`@8��@8Q�@81'@8 �@8b@8  @7�@7l�@7
=@6ȴ@6�R@6��@6��@6ff@6V@6E�@6$�@5��@5?}@4��@4z�@49X@3�
@3��@3t�@3dZ@3S�@3C�@333@3"�@3o@2�!@2~�@2�@1��@1�#@1��@1��@1x�@17L@1%@0��@0Ĝ@0�9@0�u@0bN@01'@/�;@/l�@/K�@/�@.��@.�@.��@.�+@.v�@.ff@.5?@-@-�-@-�-@-�@-�@,��@,�@,�D@,9X@,�@+�m@+�
@+ƨ@+�F@+dZ@+"�@+o@*�H@*��@*�\@*J@)�^@)��@)�7@)G�@)%@(A�@(b@'�;@'��@'�P@'\)@'�@&��@&�y@&��@&ff@&5?@&@%��@%�@%`B@%/@$�@$�j@$�D@$I�@$�@#t�@#o@"�!@"=q@"J@!��@!��@!x�@!�@!%@ �`@ �u@ r�@ A�@ b@�@l�@��@ȴ@��@�+@ff@5?@�@�T@@��@p�@�@�@�D@�D@I�@1@t�@C�@�H@~�@^5@�@��@��@hs@%@��@��@�u@Q�@�@�;@�w@�@�P@|�@l�@l�@+@
=@ȴ@��@��@�+@V@�@��@�@p�@`B@O�@?}@V@�j@�@��@z�@(�@1@��@�
@�F@��@t�@dZ@S�@C�@�@��@��@�!@��@�\@~�@�@�#@�#@��@��@hs@7L@��@�9@��@�u@�@r�@bN@Q�@A�@b@b@  @�w@��@|�@;d@
=@�@ȴ@�R@�+@�+@E�@5?@$�@��@�h@`B@V@��@z�@Z@9X@9X@�@��@�
@ƨ@��@t�@C�@"�@@
��@
~�@
^5@
=q@
�@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�-B
�9B
ǮBBW
B��B��B��B�RB�wB��B�
B�5B�ZB��B%�B;dB@�BF�BD�BB�BA�BC�BE�BF�BH�BK�BP�BXB^5B`BBcTBhsBk�Bl�Bm�Bs�Bx�Bz�B~�B�+B�VB��B��B��B�{B�\B�B�B� B�B��B��B�uB�=B}�Bt�Bn�BgmBbNB^5B[#BXBT�BN�BB�B)�B!�B�B+B�yB�/B��B�XB��B�\Bs�B[#BK�B;dBT�BcTBXBB�B=qB�B
��B
�yB
�TB
�TB
�BB
�#B
B
��B
O�B
%�B
B	�BB	��B	��B	��B	�/B	ƨB	�FB	�'B	�B	�B	��B	��B	��B	��B	��B	�B	q�B	^5B	VB	N�B	K�B	D�B	:^B	&�B	�B	B�B�B�BB��B�wB�3B��B��B��B��B�uB�B{�Bv�Bt�Bs�Bl�Bl�BhsBhsBiyBiyBdZBcTBbNBaHBaHB_;B^5B^5B^5B^5B_;B_;B]/B[#BT�BT�BR�BO�BN�BM�BJ�BG�BD�BA�B@�B@�B?}B?}B<jB>wB:^B9XB8RB7LB8RB8RB6FB5?B6FB5?B49B49B33B33B2-B0!B/B.B/B/B-B/B-B,B)�B,B(�B(�B)�B'�B'�B(�B(�B'�B)�B'�B(�B&�B%�B"�B!�B!�B �B!�B#�B!�B!�B"�B"�B"�B#�B$�B$�B#�B$�B&�B%�B'�B&�B'�B)�B+B.B-B-B-B,B-B-B-B/B/B0!B1'B49B;dB<jB<jB=qBA�B@�BA�BD�BE�BG�BJ�BJ�BK�BN�BR�BR�BR�BVBW
BZB\)B^5B_;BbNBffBgmBhsBiyBk�Bm�Bp�Bs�Bt�Bv�Bw�By�B}�B�B�+B�7B�=B�JB�\B�hB�oB�oB��B��B��B��B��B��B�B�3B�?B�LB�RB�dB�wBBȴB��B��B��B��B��B��B��B�B�5B�NB�TB�ZB�fB�fB�mB�B�B�B�B�B��B��B��B	  B	B	B	B	B	B	B	1B		7B	DB	VB	hB	oB	oB	�B	�B	�B	#�B	&�B	+B	-B	1'B	33B	8RB	9XB	;dB	>wB	@�B	E�B	J�B	N�B	P�B	R�B	VB	W
B	XB	YB	ZB	[#B	\)B	_;B	aHB	gmB	jB	l�B	m�B	p�B	q�B	u�B	x�B	y�B	z�B	|�B	�B	�+B	�7B	�=B	�=B	�DB	�JB	�PB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�LB	�LB	�XB	�^B	�jB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
VB
\B
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
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
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
(�B
(�B
'�B
'�B
'�B
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
.B
.B
.B
/B
/B
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
33B
33B
33B
33B
49B
33B
49B
5?B
5?B
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
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
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
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
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
E�B
F�B
F�B
F�B
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
I�B
I�B
I�B
I�B
J�B
J�B
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
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
W
B
W
B
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
]/B
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
`BB
`BB
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
m�B
m�B
m�B
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�zB�BV�B�SB�mB�_B�B�BB�oB��B�B�&B��B%�B;0B@iBFtBDgBBuBAUBCaBEmBFtBH�BK�BP�BW�B^B`Bc Bh>BkQBlWBmwBs�Bx�Bz�B~�B��B�"B�eB�kB�yB�FB�(B��B��B�B��B�eB�eB�@B�	B}�Bt�BncBgRBbB^BZ�BW�BT�BN�BB[B)�B!�BqBB�DB��BΥB�$B��B�(Bs�B[	BK�B;0BT�Bc:BW�BB[B=<BqB
��B
�DB
� B
� B
�B
��B
�[B
��B
O�B
%�B
 �B	�B	ҽB	ѷB	ʌB	��B	�tB	�B	��B	��B	��B	��B	��B	��B	��B	�SB	��B	qvB	^B	U�B	N�B	K�B	DgB	:*B	&�B	MB	 �B�oB�KB�BΥB�BB��B��B��B�xB�eB�@B��B{�Bv�Bt�Bs�BlWBlWBh>Bh>BiDBiDBd&Bc Ba�BaBaB_B]�B^B^B^B_B_B\�BZ�BT�BT�BR�BO�BN�BM�BJrBGzBDMBAUB@4B@OB?HB?.B<6B>BB:B9$B8B6�B8B8B6B5B5�B4�B4B4B2�B2�B1�B/�B.�B-�B.�B.�B,�B.�B,�B+�B)�B+�B(�B(�B)�B'�B'�B(�B(�B'�B)�B'�B(�B&�B%�B"�B!|B!�B �B!�B#�B!�B!�B"�B"�B"�B#�B$�B$�B#�B$�B&�B%�B'�B&�B'�B)�B*�B-�B,�B,�B,�B+�B,�B,�B,�B.�B.�B/�B0�B4B;0B<B<6B="BA;B@4BA;BDMBEmBGzBJrBJrBKxBN�BR�BR�BR�BU�BV�BY�B[�B^B_BbBfBg8Bh>Bi*BkQBmCBpoBs�Bt�Bv�Bw�By�B}�B��B��B�B�	B�B�B�4B�:B�:B�9B�eB�~B�|B��B��B��B��B��B�B�B�B�BB�AB�fBʌB̘B̈́BϫBЗBѷBҽBյB��B�B� B�&B�2B�B�8B�KB�WB�cB�[B�aB�nB��B��B��B	 �B	 �B	�B	�B	�B	�B	�B		B	
�B	"B	4B	:B	 B	9B	QB	pB	#�B	&�B	*�B	,�B	0�B	2�B	8B	9	B	;B	>(B	@4B	EmB	J�B	N�B	P�B	R�B	U�B	V�B	W�B	X�B	Y�B	Z�B	[�B	_B	`�B	g8B	j0B	lWB	m]B	poB	q[B	u�B	x�B	y�B	z�B	|�B	��B	��B	�B	�	B	�	B	��B	�B	�B	�.B	�4B	� B	�FB	�YB	�KB	�qB	�dB	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�*B	�6B	�"B	�.B	�OB	�[B	�aB	�gB	�SB	�mB	ȀB	�lB	�lB	ʌB	�rB	�rB	˒B	�~B	̈́B	ϫB	ϑB	ѝB	ѝB	ѝB	ѝB	ҽB	ңB	өB	��B	յB	յB	ּB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�8B	�>B	�DB	�6B	�QB	�QB	�=B	�cB	�cB	�OB	�OB	�oB	�[B	�|B	�|B	�aB	�|B	�hB	�B	�nB	�tB	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

	B
B

�B

�B
B
B

�B

�B

�B
�B
B
B
B
B
"B
B
.B
4B
4B
B
B
:B
:B
 B
 B
 B
 B
:B
:B
@B
FB
,B
2B
2B
MB
MB
MB
SB
SB
9B
SB
YB
_B
_B
KB
eB
kB
kB
QB
kB
WB
]B
xB
xB
xB
xB
xB
dB
dB
dB
jB
pB
�B
�B
pB
pB
 �B
 �B
 �B
 �B
!|B
!|B
!�B
!|B
!�B
"�B
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
(�B
(�B
'�B
'�B
'�B
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
-�B
-�B
-�B
.�B
.�B
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
2�B
2�B
2�B
2�B
4B
2�B
3�B
4�B
5B
5�B
6B
5�B
5�B
6B
6�B
6�B
6�B
7B
8B
8B
8B
9$B
9	B
:B
:*B
:*B
:*B
;0B
;B
;0B
<6B
<6B
=<B
="B
>(B
>(B
>BB
>BB
?HB
?HB
?HB
?HB
?.B
@OB
@4B
@OB
AUB
AUB
BAB
BAB
BAB
BAB
CaB
CGB
CaB
CGB
CaB
DMB
DgB
CaB
CGB
DgB
DMB
DMB
DgB
DMB
DgB
DMB
EmB
EmB
EmB
ESB
EmB
ESB
ESB
FYB
FtB
FtB
G_B
GzB
HfB
H�B
H�B
I�B
I�B
I�B
IlB
I�B
IlB
I�B
I�B
I�B
J�B
J�B
KxB
K�B
K�B
KxB
KxB
K�B
KxB
L~B
L~B
L�B
L~B
L~B
L~B
L~B
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
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
V�B
V�B
V�B
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
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
_B
^�B
^�B
_B
_�B
_�B
`�B
`�B
a�B
a�B
bB
bB
c B
c B
cB
c B
cB
cB
cB
c B
d&B
d&B
e,B
eB
e,B
e,B
eB
e,B
e,B
f2B
fB
f2B
f2B
fB
gB
g8B
gB
g8B
gB
gB
gB
h$B
h>B
h>B
h>B
h$B
iDB
i*B
i*B
iDB
i*B
i*B
i*B
iDB
i*B
jKB
jKB
jKB
j0B
jKB
j0B
jKB
j0B
jKB
kQB
k6B
k6B
k6B
k6B
lWB
lWB
l=B
l=B
lWB
l=B
lWB
lWB
l=B
l=B
l=B
lWB
m]B
m]B
mCB
m]B
m]B
mCB
m]B
mCB
nIB
nIB
ncB
ncB
nIB
ncB
ncB
oiB
oiB
oiB
oOB
pUB
poB
poB
pUB
poB
poB
poB
poB
poB
qvB
q[B
qvB
q[B
q[B
raB
r|B
raB
raB
r|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.49(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804150045022018041500450220180415004502201804261731542018042617315420180426173154JA  ARFMdecpA19c                                                                20180410033518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180409183546  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180409183547  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180409183547  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180409183548  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180409183548  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180409183548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180409183548  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180409183548  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180409183548                      G�O�G�O�G�O�                JA  ARUP                                                                        20180409185631                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180410154509  CV  JULD            G�O�G�O�F��>                JM  ARCAJMQC2.0                                                                 20180414154502  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180414154502  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426083154  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                