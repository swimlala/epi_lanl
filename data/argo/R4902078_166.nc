CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-11-12T13:00:32Z creation      
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
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191112130032  20191112130032  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @��YB�h�1   @��[βNB@+�$�/��d��9Xb1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`ffBg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @X��@��@ϮA�
A'�
AG�
Ag�
A��A��A��A��RA��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BJ\)BQ��BY��Bb\)Bi�]Bq�]By��B���B���B���B���B���B���B���B�ǮB���B���B���B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8�C:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�L{D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�DÏ�D�ϮD��D�O�Dď�D�ϮD��D�O�Dŏ�D�ϮD��D�O�DƏ�D�ϮD��D�O�DǏ�D�ϮD��D�O�Dȏ�D�ϮD��D�O�Dɏ�D�ϮD��D�O�Dʏ�D�ϮD��D�O�Dˏ�D�ϮD��D�O�D̏�D�ϮD��D�O�D͏�D�ϮD��D�O�DΏ�D�ϮD��D�O�DϏ�D�ϮD��D�O�DЏ�D�ϮD��D�O�Dя�D�ϮD��D�O�Dҏ�D�ϮD��D�O�Dӏ�D�ϮD��D�O�Dԏ�D�ϮD��D�O�DՏ�D�ϮD��D�O�D֏�D�ϮD��D�O�D׏�D�ϮD��D�O�D؏�D�ϮD��D�O�Dُ�D�ϮD��D�O�Dڏ�D�ϮD��D�O�Dۏ�D�ϮD��D�O�D܏�D�ϮD��D�O�Dݏ�D�ϮD��D�O�Dޏ�D�ϮD��D�O�Dߏ�D�ϮD��D�O�D���D�ϮD��D�O�DᏮD�ϮD��D�O�D⏮D�ϮD��D�O�D㏮D�ϮD��D�O�D䏮D�ϮD��D�O�D叮D�ϮD��D�O�D揮D�ϮD��D�O�D珮D�ϮD��D�O�D菮D�ϮD��D�O�D鏮D�ϮD��D�O�DꏮD�ϮD��D�O�D돮D�ϮD��D�O�D쏮D�ϮD��D�O�D폮D�ϮD��D�O�DD�ϮD��D�O�DD�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�R�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A䝲A䙚A��A��A��A��A�A�A�A�A�-A�!A�!A�!A�A�-A�9A�!A�!A�!A�-A�9A�9A�A䟾A䗍A�\A�+A�O�A�`BAں^A֑hAՏ\A�K�A���A�p�A�|�A���A���A��AȅA�x�A�&�Aĺ^A�(�A���A�5?A�-A�=qA�;dA�Q�A�A�S�A��/A�1A��mA�v�A���A��/A���A��FA��A�hsA���A�?}A��`A���A���A�A�A��A�bA���A�$�A��RA�S�A�^5A�x�A���A���A���A���A��A�-A���A��mA�;dA���A��\A���A��
A��AvZAr��Aj�Aa�wA_p�A^��A^5?A\�\AZ�AV�DAM\)AJ5?AI�AH�RAG�AFz�AA"�A=dZA9�TA7�A5�A5l�A4��A4jA3�A3G�A3l�A2�A1ƨA1�A0z�A/��A/p�A.��A.�\A-��A,ZA*��A)dZA(�/A(=qA'�#A'+A%��A#|�A!G�A �+A��A/A�/A~�AbA��A  A�A��A�hA
=A�
A��A��AO�A��AE�AA`BA�HA�AoA��A��A�`A�A��AhsA�A��A�uAA�A`BA{A�A��A��A��A�A�uA�Ar�An�AbNA  A�-Av�A5?A(�A �A$�A{A��A�#A�-A��A�FA
��A
��A
�A
=AA
�A
-A	�A	t�A	l�A	+A�/A��AffAhsA��AoA�!Ar�AbA\)A�jA�#A��A�wA��A��A|�A ��A 5?@��@�K�@���@��7@��@��u@��m@���@���@�X@��9@��D@�bN@�(�@�1@��;@�@�@�z�@��
@���@�n�@�@�O�@�u@��
@���@�$�@�D@�5?@��T@�7@�9@�r�@�Z@�  @�l�@�+@�~�@�=q@��#@�hs@��`@�  @㝲@�@�!@�7L@�Ĝ@��@�j@�1'@�1@ߕ�@��H@ޗ�@�E�@��@���@� �@���@۶F@۶F@�\)@�V@��@ّh@��/@�Q�@�  @׾w@�"�@�@�`B@�&�@�bN@�33@���@��H@ҸR@�^5@�-@�Z@�J@�%@̋D@̓u@̛�@̣�@̛�@˕�@�~�@�^5@��@��@ɺ^@ɡ�@�`B@���@ǝ�@ư!@�=q@��#@�X@�Ĝ@��@�dZ@�+@�o@�
=@�@��@��@°!@�@+@�^5@��@���@�O�@���@���@�1'@�  @���@�l�@�C�@�@��@��!@�^5@�M�@�-@��@�`B@��j@��@�\)@�;d@�33@�"�@�o@��@���@�$�@�@���@��h@�/@��9@�j@�9X@���@�ƨ@���@��@�+@�ȴ@��!@��+@�^5@�{@���@�x�@�`B@�V@��9@�Q�@��@���@�K�@���@�{@��@��^@��-@���@�hs@��@�1@��;@��;@���@�S�@�;d@��@���@���@� �@���@�ƨ@���@��@�dZ@��H@�E�@��@��-@��@�hs@�`B@�7L@���@�1'@��F@�l�@��@��@���@�5?@�@�hs@�?}@��@��/@�z�@�b@�K�@��@���@�M�@���@��@��j@��9@��D@�I�@��@�  @��;@���@�ƨ@���@�+@��H@��!@��+@�^5@�@��T@��T@��h@�G�@���@�  @���@�dZ@�+@��y@���@�ff@�J@���@��@��^@���@�p�@�7L@��@��u@�  @��P@�"�@��\@��+@�=q@�@��7@�X@��`@��@���@��u@�r�@�Q�@�I�@� �@���@��;@��
@��
@���@���@�ƨ@��@��@���@�=q@��@�G�@���@���@�bN@��m@��@�+@��@���@�$�@�{@�J@���@��@��T@���@�x�@�/@�/@�&�@���@�r�@�j@�j@�I�@�  @��F@��P@�C�@�ȴ@���@�n�@�5?@��@�J@��@���@��h@�X@�&�@��@���@���@�Z@�  @��F@�\)@���@���@�=q@��-@�hs@�G�@��`@�r�@�(�@�b@���@���@��m@�ƨ@��@���@��P@�l�@��@��\@�{@��#@��7@��@��9@���@��u@��D@��D@��D@�z�@�j@��@�@~��@}�h@}O�@}O�@}O�@}�@{��@z^5@y�7@y&�@x��@x�9@xr�@w
=@vv�@v5?@v@v@v@u�@u�@u�@u�T@u@u�@t�/@t��@t(�@s�m@s��@s"�@r��@r~�@rJ@q�#@q��@q�7@q�@pQ�@o�@o��@o��@ol�@o\)@n��@n{@m�-@m`B@m/@l��@l1@j��@i��@iX@h��@hr�@g�@f��@f�+@f$�@f@e�T@e�-@eO�@d�@d(�@co@a�@ax�@`�u@_;d@^��@^E�@]`B@\�j@\j@[ƨ@[dZ@[@Zn�@Y�^@YX@X��@XbN@W\)@V�+@VV@V5?@V$�@V{@V@U�@U��@Up�@T��@T�/@T�@T9X@S�
@R�H@R�!@R��@R�\@Rn�@RM�@R=q@R-@R�@Q��@Q�#@Q��@Qhs@Q&�@Q�@P��@P�@O�P@O+@N�y@N�R@Nv�@N5?@M�h@L��@LZ@K�m@K�
@K�F@K�F@K�@KC�@K@J�!@Jn�@J�@I��@H�`@HA�@G�@G�@G�P@GK�@F�+@F{@F{@E�@E@E�-@Ep�@EV@D�D@D�@C��@B��@B�\@B^5@BM�@BM�@B=q@B-@BJ@A�@A�^@@�`@>v�@=�-@=�h@=�@=�@=�@=�@=p�@=?}@=�@<�@;�
@;��@;S�@;o@:�@:^5@9�^@9G�@8��@7�@7K�@6ȴ@6E�@6@5�@5��@5?}@4�@4�D@4j@4Z@4I�@4I�@49X@4�@3��@3ƨ@3ƨ@3�@3"�@3@2�H@2��@2~�@2n�@2M�@2�@1��@1��@1�7@1hs@1X@17L@0�9@.V@-�@-@-�-@-�h@-�h@-`B@-O�@-?}@-�@-V@,�@,�/@,�@,��@,��@,z�@,1@+��@+"�@*�H@*��@*�\@*=q@)�#@(A�@'�@'�P@'l�@'K�@'+@&��@&��@&E�@&@%�T@%��@%@%�-@%��@%?}@$�@$��@$j@$Z@#��@#t�@#33@"��@"~�@"M�@!��@!&�@!&�@!�@!%@ �u@ 1'@ b@�w@;d@�R@`B@?}@?}@/@�@�@/@/@?}@?}@?}@?}@�@z�@�m@��@��@��@dZ@�\@M�@J@�@�#@�^@��@hs@7L@&�@�@��@��@�@Q�@A�@�;@��@|�@\)@\)@K�@;d@
=@�R@v�@ff@5?@$�@�T@�-@�@?}@V@�@�@�D@j@Z@9X@��@��@t�@S�@"�@�H@�!@�\@~�@n�@^5@=q@�@�@�@J@��@��@�#@�#@�^@��@��@�7@x�@x�@7L@%@Ĝ@�9@�u@r�@Q�@1'@�@�@�P@|�@|�@l�@l�@\)@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A䝲A䙚A��A��A��A��A�A�A�A�A�-A�!A�!A�!A�A�-A�9A�!A�!A�!A�-A�9A�9A�A䟾A䗍A�\A�+A�O�A�`BAں^A֑hAՏ\A�K�A���A�p�A�|�A���A���A��AȅA�x�A�&�Aĺ^A�(�A���A�5?A�-A�=qA�;dA�Q�A�A�S�A��/A�1A��mA�v�A���A��/A���A��FA��A�hsA���A�?}A��`A���A���A�A�A��A�bA���A�$�A��RA�S�A�^5A�x�A���A���A���A���A��A�-A���A��mA�;dA���A��\A���A��
A��AvZAr��Aj�Aa�wA_p�A^��A^5?A\�\AZ�AV�DAM\)AJ5?AI�AH�RAG�AFz�AA"�A=dZA9�TA7�A5�A5l�A4��A4jA3�A3G�A3l�A2�A1ƨA1�A0z�A/��A/p�A.��A.�\A-��A,ZA*��A)dZA(�/A(=qA'�#A'+A%��A#|�A!G�A �+A��A/A�/A~�AbA��A  A�A��A�hA
=A�
A��A��AO�A��AE�AA`BA�HA�AoA��A��A�`A�A��AhsA�A��A�uAA�A`BA{A�A��A��A��A�A�uA�Ar�An�AbNA  A�-Av�A5?A(�A �A$�A{A��A�#A�-A��A�FA
��A
��A
�A
=AA
�A
-A	�A	t�A	l�A	+A�/A��AffAhsA��AoA�!Ar�AbA\)A�jA�#A��A�wA��A��A|�A ��A 5?@��@�K�@���@��7@��@��u@��m@���@���@�X@��9@��D@�bN@�(�@�1@��;@�@�@�z�@��
@���@�n�@�@�O�@�u@��
@���@�$�@�D@�5?@��T@�7@�9@�r�@�Z@�  @�l�@�+@�~�@�=q@��#@�hs@��`@�  @㝲@�@�!@�7L@�Ĝ@��@�j@�1'@�1@ߕ�@��H@ޗ�@�E�@��@���@� �@���@۶F@۶F@�\)@�V@��@ّh@��/@�Q�@�  @׾w@�"�@�@�`B@�&�@�bN@�33@���@��H@ҸR@�^5@�-@�Z@�J@�%@̋D@̓u@̛�@̣�@̛�@˕�@�~�@�^5@��@��@ɺ^@ɡ�@�`B@���@ǝ�@ư!@�=q@��#@�X@�Ĝ@��@�dZ@�+@�o@�
=@�@��@��@°!@�@+@�^5@��@���@�O�@���@���@�1'@�  @���@�l�@�C�@�@��@��!@�^5@�M�@�-@��@�`B@��j@��@�\)@�;d@�33@�"�@�o@��@���@�$�@�@���@��h@�/@��9@�j@�9X@���@�ƨ@���@��@�+@�ȴ@��!@��+@�^5@�{@���@�x�@�`B@�V@��9@�Q�@��@���@�K�@���@�{@��@��^@��-@���@�hs@��@�1@��;@��;@���@�S�@�;d@��@���@���@� �@���@�ƨ@���@��@�dZ@��H@�E�@��@��-@��@�hs@�`B@�7L@���@�1'@��F@�l�@��@��@���@�5?@�@�hs@�?}@��@��/@�z�@�b@�K�@��@���@�M�@���@��@��j@��9@��D@�I�@��@�  @��;@���@�ƨ@���@�+@��H@��!@��+@�^5@�@��T@��T@��h@�G�@���@�  @���@�dZ@�+@��y@���@�ff@�J@���@��@��^@���@�p�@�7L@��@��u@�  @��P@�"�@��\@��+@�=q@�@��7@�X@��`@��@���@��u@�r�@�Q�@�I�@� �@���@��;@��
@��
@���@���@�ƨ@��@��@���@�=q@��@�G�@���@���@�bN@��m@��@�+@��@���@�$�@�{@�J@���@��@��T@���@�x�@�/@�/@�&�@���@�r�@�j@�j@�I�@�  @��F@��P@�C�@�ȴ@���@�n�@�5?@��@�J@��@���@��h@�X@�&�@��@���@���@�Z@�  @��F@�\)@���@���@�=q@��-@�hs@�G�@��`@�r�@�(�@�b@���@���@��m@�ƨ@��@���@��P@�l�@��@��\@�{@��#@��7@��@��9@���@��u@��D@��D@��D@�z�@�j@��@�@~��@}�h@}O�@}O�@}O�@}�@{��@z^5@y�7@y&�@x��@x�9@xr�@w
=@vv�@v5?@v@v@v@u�@u�@u�@u�T@u@u�@t�/@t��@t(�@s�m@s��@s"�@r��@r~�@rJ@q�#@q��@q�7@q�@pQ�@o�@o��@o��@ol�@o\)@n��@n{@m�-@m`B@m/@l��@l1@j��@i��@iX@h��@hr�@g�@f��@f�+@f$�@f@e�T@e�-@eO�@d�@d(�@co@a�@ax�@`�u@_;d@^��@^E�@]`B@\�j@\j@[ƨ@[dZ@[@Zn�@Y�^@YX@X��@XbN@W\)@V�+@VV@V5?@V$�@V{@V@U�@U��@Up�@T��@T�/@T�@T9X@S�
@R�H@R�!@R��@R�\@Rn�@RM�@R=q@R-@R�@Q��@Q�#@Q��@Qhs@Q&�@Q�@P��@P�@O�P@O+@N�y@N�R@Nv�@N5?@M�h@L��@LZ@K�m@K�
@K�F@K�F@K�@KC�@K@J�!@Jn�@J�@I��@H�`@HA�@G�@G�@G�P@GK�@F�+@F{@F{@E�@E@E�-@Ep�@EV@D�D@D�@C��@B��@B�\@B^5@BM�@BM�@B=q@B-@BJ@A�@A�^@@�`@>v�@=�-@=�h@=�@=�@=�@=�@=p�@=?}@=�@<�@;�
@;��@;S�@;o@:�@:^5@9�^@9G�@8��@7�@7K�@6ȴ@6E�@6@5�@5��@5?}@4�@4�D@4j@4Z@4I�@4I�@49X@4�@3��@3ƨ@3ƨ@3�@3"�@3@2�H@2��@2~�@2n�@2M�@2�@1��@1��@1�7@1hs@1X@17L@0�9@.V@-�@-@-�-@-�h@-�h@-`B@-O�@-?}@-�@-V@,�@,�/@,�@,��@,��@,z�@,1@+��@+"�@*�H@*��@*�\@*=q@)�#@(A�@'�@'�P@'l�@'K�@'+@&��@&��@&E�@&@%�T@%��@%@%�-@%��@%?}@$�@$��@$j@$Z@#��@#t�@#33@"��@"~�@"M�@!��@!&�@!&�@!�@!%@ �u@ 1'@ b@�w@;d@�R@`B@?}@?}@/@�@�@/@/@?}@?}@?}@?}@�@z�@�m@��@��@��@dZ@�\@M�@J@�@�#@�^@��@hs@7L@&�@�@��@��@�@Q�@A�@�;@��@|�@\)@\)@K�@;d@
=@�R@v�@ff@5?@$�@�T@�-@�@?}@V@�@�@�D@j@Z@9X@��@��@t�@S�@"�@�H@�!@�\@~�@n�@^5@=q@�@�@�@J@��@��@�#@�#@�^@��@��@�7@x�@x�@7L@%@Ĝ@�9@�u@r�@Q�@1'@�@�@�P@|�@|�@l�@l�@\)@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�dB�jB�dB�dB�dB�jB�jB�qB�wB�wB��B	�B	�bB	�B
	7B
%�B
%�B
-B
1'B
$�B
)�B
F�B
m�B
�1B
�jBB=qBdZBp�B�B�B�VB��B�'B�9B�?B�?B�9B��B��B��B��B��B��BȴB�dB�-B�B�!B��B�oB�JB�1B�B� B}�By�Bm�BN�B/B#�B�B	7B
��B
�)B
�B
�%B
o�B
gmB
bNB
S�B
8RB
�B	�B	�B	�9B	�uB	�JB	�=B	�+B	�B	{�B	t�B	jB	gmB	gmB	ffB	dZB	_;B	]/B	\)B	R�B	ZB	dZB	dZB	bNB	dZB	q�B	� B	�JB	��B	�B	�FB	�FB	�9B	�3B	�?B	�XB	��B	B	��B	�
B	�#B	�NB	�mB	�B	�sB	�#B	��B	��B	��B	��B	��B	��B	ɺB	��B	�;B	�sB	�B	��B	��B	�B	�B	�B	�mB	�ZB	�sB	�sB	�NB	��B	B	B	��B	��B	ÖB	ŢB	��B	�ZB	�B	�yB	�sB	�`B	�BB	�5B	�BB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�fB	�fB	�fB	�fB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B
  B
1B

=B
JB
DB

=B
DB
JB
PB
DB

=B
DB
hB
bB
PB
JB

=B
	7B
+B
B
B

=B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
hB
uB
�B
�B
�B
�B
�B
{B
hB
PB
bB
�B
{B
uB
hB
bB
bB
bB
hB
\B
PB
JB
JB
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

=B

=B
JB
JB
JB
VB
VB
PB
PB
JB
DB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
uB
{B
{B
{B
uB
oB
oB
oB
uB
hB
hB
oB
hB
bB
hB
uB
{B
{B
uB
{B
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
uB
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
oB
hB
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
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
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
'�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
(�B
)�B
)�B
+B
+B
,B
,B
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
-B
.B
.B
.B
/B
0!B
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
2-B
2-B
2-B
33B
33B
2-B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
49B
49B
5?B
6FB
6FB
6FB
6FB
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
;dB
;dB
;dB
;dB
<jB
<jB
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
?}B
?}B
?}B
?}B
?}B
@�B
A�B
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
C�B
D�B
E�B
F�B
F�B
F�B
F�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
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
O�B
N�B
N�B
O�B
N�B
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
Q�B
R�B
S�B
S�B
S�B
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
YB
ZB
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
]/B
]/B
]/B
]/B
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
^5B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
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
dZB
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
ffB
gmB
gmB
gmB
gmB
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
o�B
o�B
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
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�7B
�7B
�DB
�DB
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
�VB
�VB
�VB
�VB
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
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�&B�,B�,B�8B	gB	�B	�eB	��B
�B
�B
�B
 �B
�B
�B
6]B
]FB
w�B
�B
��B-&BTB`YBp�Bt�B~B��B��B��B��B��B��B�|B��B��B��B§B��B�iB�B��B��B��B��B�$B{�Bw�Br�Bo�Bm�Bi�B]FB>�B�B�B6B
��B
�B
��B
��B
u�B
_SB
W"B
RB
C�B
(B
[B	�@B	ŹB	��B	�*B	{�B	y�B	v�B	q�B	k�B	dqB	Z4B	W"B	W"B	VB	TB	N�B	L�B	K�B	B�B	I�B	TB	TB	RB	TB	a_B	o�B	{�B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�DB	��B	ƿB	��B	�B	�"B	�4B	�(B	��B	��B	��B	��B	��B	��B	��B	�oB	��B	��B	�(B	�YB	�B	�B	�@B	�MB	�@B	�"B	�B	�(B	�(B	�B	��B	�DB	�DB	�>B	�>B	�KB	�WB	��B	�B	�4B	�.B	�(B	�B	��B	��B	��B	�B	�B	�B	�B	�"B	�(B	�.B	�.B	�:B	�@B	�:B	�B	�B	�B	�B	�B	�B	�(B	�(B	�.B	�:B	�MB	�:B	�YB	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B
B
 B	�B	��B	��B	��B	��B	��B	��B	��B
*B
HB

UB
[B
aB
aB
[B

UB
HB
6B
<B
<B
0B
$B
 B
B
*B
6B
<B
BB
BB
BB
0B
B	�B
 B
<B
0B
*B
B
 B
 B
 B
B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B
*B
0B
0B
0B
*B
$B
$B
$B
*B
B
B
$B
B
 B
B
*B
0B
0B
*B
0B
*B
*B
0B
0B
6B
6B
<B
HB
BB
BB
BB
<B
<B
<B
6B
6B
6B
0B
*B
*B
*B
$B
$B
$B
$B
$B
$B
$B
$B
$B
$B
$B
$B
$B
B
B
B
B
B
$B
$B
$B
$B
$B
$B
*B
*B
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
6B
6B
6B
<B
HB
	OB
	OB
	OB
	OB
	OB

UB

UB

UB

UB
[B
[B
[B
[B
aB
aB
aB
gB
gB
aB
[B

UB
	OB
	OB
	OB

UB
[B
HB
HB
	OB
[B
[B
[B
[B
[B
	OB
	OB
	OB

UB

UB

UB

UB

UB
aB
gB
gB
mB
mB
mB
mB
mB
mB
sB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
!�B
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
#�B
#�B
$�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
(B
(B
)B
)B
*B
*B
*B
*B
+B
+B
+B
+B
+B
+B
+B
,B
,B
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
-&B
.,B
.,B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
/2B
08B
1>B
2DB
2DB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
4QB
5WB
6]B
6]B
6]B
6]B
8iB
8iB
9oB
9oB
9oB
9oB
9oB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
;|B
;|B
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
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
>�B
>�B
?�B
>�B
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
A�B
B�B
C�B
C�B
C�B
E�B
E�B
E�B
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
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
RB
S	B
S	B
S	B
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
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
W"B
VB
W"B
W"B
W"B
W"B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
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
]FB
]FB
]FB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
_SB
_SB
`YB
`YB
a_B
a_B
a_B
a_B
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
ckB
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
dqB
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
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
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
j�B
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
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
q�B
q�B
q�B
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
s�B
s�B
s�B
s�B
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
r�B
s�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
z�B
z�B
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
~B
~B
~B
~B
~B
~B
~B
~B
~B
~B
B
B
B
B
B
~B
~B
B
B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.49 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20191112130032              20191112130032  AO  ARCAADJP                                                                    20191112130032    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20191112130032    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191112130032  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191112130032  QCF$                G�O�G�O�G�O�0               