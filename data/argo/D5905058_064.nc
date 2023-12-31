CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-31T00:35:10Z creation;2018-05-31T00:35:12Z conversion to V3.1;2019-12-23T06:21:04Z update;     
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �$   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180531003510  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA  I2_0675_064                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�f��"� 1   @�f�\�$ @8! [�7�c��w�k1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3��D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De��Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl�Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�R�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�R�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD�{D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D��{D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�DÏ�D�ϮD��D�O�Dď�D�ϮD��D�O�Dŏ�D�ϮD��D�O�DƏ�D�ϮD��D�O�DǏ�D�ϮD��D�O�Dȏ�D�ϮD��D�O�Dɏ�D�ϮD��D�O�Dʏ�D�ϮD��D�O�Dˏ�D�ϮD��D�O�D̏�D�ϮD��D�O�D͏�D�ϮD��D�O�DΏ�D�ϮD��D�O�DϏ�D�ϮD��D�O�DЏ�D�ϮD��D�O�Dя�D�ϮD��D�O�Dҏ�D�ϮD��D�O�Dӏ�D�ϮD��D�O�Dԏ�D�ϮD��D�O�DՏ�D�ϮD��D�O�D֏�D�ϮD��D�O�D׏�D�ϮD��D�O�D؏�D�ϮD��D�O�Dُ�D�ϮD��D�O�Dڏ�D�ϮD��D�O�Dۏ�D�ϮD��D�O�D܏�D�ϮD��D�O�Dݏ�D�ϮD��D�O�Dޏ�D�ϮD��D�O�Dߏ�D�ϮD��D�O�D���D�ϮD��D�O�DᏮD�ϮD��D�O�D⏮D�ϮD��D�O�D㏮D�ϮD��D�O�D䏮D�ϮD��D�O�D叮D�ϮD��D�O�D揮D�ϮD��D�O�D珮D�ϮD��D�O�D菮D�ϮD��D�O�D鏮D���D��D�O�DꏮD�ϮD��D�O�D돮D�ϮD��D�O�D쏮D�ϮD��D�O�D폮D�ϮD��D�O�DD�ϮD��D�O�DD�ϮD��D�O�D���D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D�D�ϮD��D�O�D���D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��+A��A�~�A��A�|�A�p�A�\)A���A���A��-A�XA��;A�
=A� �A���A��A��A�t�A�G�A��A��#A���A�S�A���A��-A���A��A� �A�;dA���A�=qA�&�A��HA�Q�A�Q�A��RA�=qA�9XA��A�A�jA�XA���A��A��A���A�dZA�XA�A�A��A�bA�
=A��TA��!A���A��A�ȴA��A���A�v�A�C�A�=qA�E�A�oA��
A�ƨA��!A�^5A���A���A���A�dZA��^A�t�A�n�A�VA�A�JA��A�t�A��HA�p�A�t�A�ZA��RA�33A��A�ffA��hA���A�l�A�=qA���A�XA��A���A��A��A��^A�%A�x�A��A�K�A���A�mA~��A|�9Az�jAy�PAx��Aw��AvȴAu�mAtr�Aq;dAm�Ak33Aj$�Ah~�Af�AdE�AaK�A_�A^��A[l�AZ^5AYƨAX�uAU�FAT��AS�AR�!AQ��AP-AN�HAMx�AL��AKƨAK�AKVAJ�!AJbAIl�AH�\AG+AE��AC��A@�A?A?XA=�A;��A:E�A8�A7��A6^5A5��A4�!A2ĜA1�A0E�A/ƨA.�!A-�A+��A*�`A)�;A)�A)�A(��A'�
A'�7A'�A&=qA%O�A#�A#`BA"��A!�#A!hsA �A 9XAl�A7LA�A��A��AjA��A�A��A��AbA�wAO�A��A��A{A/A��A��A��A�#AS�A�A^5A��Ax�A�9A��A
�A
Q�A	x�AbNA��A�jA��A&�A�DA�A1'At�An�A�-A �yA 1'@�
=@�&�@�"�@�$�@�7L@��m@���@�I�@�@�hs@��@�C�@�^5@���@�@�P@�ff@���@�1'@��@�M�@�hs@�Ĝ@��m@�ȴ@��@���@� �@���@�n�@ݙ�@���@��@�+@�hs@�Z@�t�@ԓu@ҏ\@�&�@Ѓ@�r�@��;@��H@��T@͉7@�9X@�K�@�v�@�/@� �@Ƈ+@�%@å�@+@���@���@�Q�@��@�=q@�O�@�A�@��
@���@��7@�?}@���@�Q�@��m@���@�o@��!@�n�@�$�@�@��^@��h@�`B@���@�j@��
@�S�@��y@�v�@��@�V@���@�Z@���@�"�@��H@���@�V@��@�  @��@�o@���@��@�hs@�&�@��`@���@��;@�dZ@�S�@��y@�-@�p�@���@��@�(�@�|�@���@�@��@��/@�A�@�+@��+@�5?@���@��7@�?}@��`@���@�Z@�Q�@�j@�Z@�Q�@�I�@��@���@��@�t�@�\)@�;d@�"�@�+@�33@��@���@��H@���@�ff@�$�@���@��^@��7@��@��@�x�@�p�@�7L@���@���@���@�Z@���@��w@���@�K�@�+@�
=@��y@��@���@��\@�M�@���@��#@��T@�@���@��@��@��+@�v�@��@��#@��@��T@���@���@�hs@�7L@���@��@��/@��@���@�z�@�1'@��;@���@��P@�t�@�K�@�
=@���@��+@�=q@��@��7@�`B@��@��@��/@���@��@�1@��
@��w@���@�|�@�l�@�33@�o@�
=@���@�ȴ@���@�E�@���@���@���@���@�x�@�/@�V@���@�9X@���@���@�|�@�+@��\@�M�@�=q@�$�@�J@��#@���@�X@�7L@�V@��`@�Ĝ@��D@�Q�@�9X@�1@��;@��@��P@�C�@��y@���@�v�@�E�@�@���@���@�X@��@���@���@��9@�j@�9X@�b@��;@���@��w@��@�\)@�@�~�@�^5@�V@�M�@�=q@�J@���@�p�@�X@�O�@���@��9@�r�@�A�@��@��@l�@
=@~�+@~@}��@}/@|�/@|j@|9X@|�@{�
@{��@{33@z�H@z��@zn�@z�@y��@yG�@x��@w�@w��@xA�@w�@w�@u�@u�T@u�-@u�@u?}@t�j@t�j@t��@tI�@s��@s��@s"�@r��@r=q@rJ@q�#@q��@q%@pA�@o|�@n��@nV@nE�@m�-@m��@m�h@m`B@m�@l�/@l��@k��@kS�@j��@i�#@h�`@g|�@g\)@g+@g
=@g
=@gK�@g;d@f��@fv�@e@d�j@c33@b-@a��@a��@ahs@`  @_
=@^ȴ@^��@^v�@^��@_+@_
=@^V@^{@^@]�T@]�@\��@\I�@[�@[33@Z�!@Z=q@Y��@Yhs@YG�@X�u@W�@W|�@WK�@W;d@W�@V��@Vv�@Vff@V@U�@UO�@U/@T��@TZ@T�@Sƨ@SdZ@S"�@R�@R�@R��@Rn�@R�@Q�@Q��@Q��@Q�7@Qx�@Qhs@QG�@P��@P�@P  @Ol�@O;d@O�@N��@N�@N�+@NV@N@M��@Mp�@L��@L(�@K�m@Kƨ@KS�@J�\@J-@JJ@I�@IX@I�@H�`@H��@H�@HbN@HA�@Hb@H  @H  @G�@G��@G�@F�R@F5?@E�@E�T@E�h@EO�@E�@E�@D��@D�j@D�D@Dj@DZ@D(�@C�
@CdZ@CS�@CC�@Co@B��@Bn�@BM�@B=q@A��@A�#@A��@A�7@Ax�@A7L@@��@@�`@@�`@@bN@@  @?�P@?l�@?�@>�R@>E�@>{@>{@=�@=�@=?}@<��@<�@<�/@<�/@<�@<z�@<Z@<Z@<I�@<(�@<�@<1@;�
@;�F@;dZ@;o@:�@:��@:�!@:��@:n�@:J@9��@9hs@9�@8��@8�@8bN@81'@8  @7��@7\)@7+@6�y@6�R@6V@6@5�h@5?}@4��@4�@4��@4��@4j@4(�@3�m@3�F@3S�@2�@2��@2n�@2�@1�#@1��@1�7@1X@0��@0�@0A�@01'@0b@/�;@/��@/�@/K�@/
=@.��@.�y@.�R@.E�@.$�@.@-��@-`B@-�@-V@,�@,��@,I�@,9X@+��@+t�@*�H@*��@*M�@*=q@)�@)�^@)��@)hs@)7L@(��@(�`@(Ĝ@(�9@(�u@(�@(bN@(1'@( �@(1'@(�u@(�u@(bN@(1'@(b@'�@'�;@'��@'�w@'��@'|�@'l�@';d@'+@&�y@&v�@%@%V@$��@$��@$Z@$Z@$I�@#��@#�F@#t�@#C�@"�@"��@"�\@"^5@"^5@"M�@"=q@"=q@"=q@"=q@"�@!�^@!x�@!G�@ �`@ Ĝ@ �u@ �@ r�@ Q�@��@l�@K�@�@��@��@�@��@�+@V@{@�T@�h@V@��@��@�D@�D@�D@��@I�@�@�@�
@dZ@"�@�@��@�\@=q@-@J@�#@��@x�@�@��@�`@��@�9@bN@  @��@�P@l�@K�@�@
=@�R@�+@V@@�T@@�h@/@��@�/@�/@�/@�/@�j@I�@(�@�@��@ƨ@�@S�@C�@33@33@33@"�@o@�@��@~�@=q@�@J@�#@��@�^@�7@X@7L@�@�@%@%@%@�`@Ĝ@�9@�@A�@A�@1'@ �@  @�@�;@��@|�@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��+A��A�~�A��A�|�A�p�A�\)A���A���A��-A�XA��;A�
=A� �A���A��A��A�t�A�G�A��A��#A���A�S�A���A��-A���A��A� �A�;dA���A�=qA�&�A��HA�Q�A�Q�A��RA�=qA�9XA��A�A�jA�XA���A��A��A���A�dZA�XA�A�A��A�bA�
=A��TA��!A���A��A�ȴA��A���A�v�A�C�A�=qA�E�A�oA��
A�ƨA��!A�^5A���A���A���A�dZA��^A�t�A�n�A�VA�A�JA��A�t�A��HA�p�A�t�A�ZA��RA�33A��A�ffA��hA���A�l�A�=qA���A�XA��A���A��A��A��^A�%A�x�A��A�K�A���A�mA~��A|�9Az�jAy�PAx��Aw��AvȴAu�mAtr�Aq;dAm�Ak33Aj$�Ah~�Af�AdE�AaK�A_�A^��A[l�AZ^5AYƨAX�uAU�FAT��AS�AR�!AQ��AP-AN�HAMx�AL��AKƨAK�AKVAJ�!AJbAIl�AH�\AG+AE��AC��A@�A?A?XA=�A;��A:E�A8�A7��A6^5A5��A4�!A2ĜA1�A0E�A/ƨA.�!A-�A+��A*�`A)�;A)�A)�A(��A'�
A'�7A'�A&=qA%O�A#�A#`BA"��A!�#A!hsA �A 9XAl�A7LA�A��A��AjA��A�A��A��AbA�wAO�A��A��A{A/A��A��A��A�#AS�A�A^5A��Ax�A�9A��A
�A
Q�A	x�AbNA��A�jA��A&�A�DA�A1'At�An�A�-A �yA 1'@�
=@�&�@�"�@�$�@�7L@��m@���@�I�@�@�hs@��@�C�@�^5@���@�@�P@�ff@���@�1'@��@�M�@�hs@�Ĝ@��m@�ȴ@��@���@� �@���@�n�@ݙ�@���@��@�+@�hs@�Z@�t�@ԓu@ҏ\@�&�@Ѓ@�r�@��;@��H@��T@͉7@�9X@�K�@�v�@�/@� �@Ƈ+@�%@å�@+@���@���@�Q�@��@�=q@�O�@�A�@��
@���@��7@�?}@���@�Q�@��m@���@�o@��!@�n�@�$�@�@��^@��h@�`B@���@�j@��
@�S�@��y@�v�@��@�V@���@�Z@���@�"�@��H@���@�V@��@�  @��@�o@���@��@�hs@�&�@��`@���@��;@�dZ@�S�@��y@�-@�p�@���@��@�(�@�|�@���@�@��@��/@�A�@�+@��+@�5?@���@��7@�?}@��`@���@�Z@�Q�@�j@�Z@�Q�@�I�@��@���@��@�t�@�\)@�;d@�"�@�+@�33@��@���@��H@���@�ff@�$�@���@��^@��7@��@��@�x�@�p�@�7L@���@���@���@�Z@���@��w@���@�K�@�+@�
=@��y@��@���@��\@�M�@���@��#@��T@�@���@��@��@��+@�v�@��@��#@��@��T@���@���@�hs@�7L@���@��@��/@��@���@�z�@�1'@��;@���@��P@�t�@�K�@�
=@���@��+@�=q@��@��7@�`B@��@��@��/@���@��@�1@��
@��w@���@�|�@�l�@�33@�o@�
=@���@�ȴ@���@�E�@���@���@���@���@�x�@�/@�V@���@�9X@���@���@�|�@�+@��\@�M�@�=q@�$�@�J@��#@���@�X@�7L@�V@��`@�Ĝ@��D@�Q�@�9X@�1@��;@��@��P@�C�@��y@���@�v�@�E�@�@���@���@�X@��@���@���@��9@�j@�9X@�b@��;@���@��w@��@�\)@�@�~�@�^5@�V@�M�@�=q@�J@���@�p�@�X@�O�@���@��9@�r�@�A�@��@��@l�@
=@~�+@~@}��@}/@|�/@|j@|9X@|�@{�
@{��@{33@z�H@z��@zn�@z�@y��@yG�@x��@w�@w��@xA�@w�@w�@u�@u�T@u�-@u�@u?}@t�j@t�j@t��@tI�@s��@s��@s"�@r��@r=q@rJ@q�#@q��@q%@pA�@o|�@n��@nV@nE�@m�-@m��@m�h@m`B@m�@l�/@l��@k��@kS�@j��@i�#@h�`@g|�@g\)@g+@g
=@g
=@gK�@g;d@f��@fv�@e@d�j@c33@b-@a��@a��@ahs@`  @_
=@^ȴ@^��@^v�@^��@_+@_
=@^V@^{@^@]�T@]�@\��@\I�@[�@[33@Z�!@Z=q@Y��@Yhs@YG�@X�u@W�@W|�@WK�@W;d@W�@V��@Vv�@Vff@V@U�@UO�@U/@T��@TZ@T�@Sƨ@SdZ@S"�@R�@R�@R��@Rn�@R�@Q�@Q��@Q��@Q�7@Qx�@Qhs@QG�@P��@P�@P  @Ol�@O;d@O�@N��@N�@N�+@NV@N@M��@Mp�@L��@L(�@K�m@Kƨ@KS�@J�\@J-@JJ@I�@IX@I�@H�`@H��@H�@HbN@HA�@Hb@H  @H  @G�@G��@G�@F�R@F5?@E�@E�T@E�h@EO�@E�@E�@D��@D�j@D�D@Dj@DZ@D(�@C�
@CdZ@CS�@CC�@Co@B��@Bn�@BM�@B=q@A��@A�#@A��@A�7@Ax�@A7L@@��@@�`@@�`@@bN@@  @?�P@?l�@?�@>�R@>E�@>{@>{@=�@=�@=?}@<��@<�@<�/@<�/@<�@<z�@<Z@<Z@<I�@<(�@<�@<1@;�
@;�F@;dZ@;o@:�@:��@:�!@:��@:n�@:J@9��@9hs@9�@8��@8�@8bN@81'@8  @7��@7\)@7+@6�y@6�R@6V@6@5�h@5?}@4��@4�@4��@4��@4j@4(�@3�m@3�F@3S�@2�@2��@2n�@2�@1�#@1��@1�7@1X@0��@0�@0A�@01'@0b@/�;@/��@/�@/K�@/
=@.��@.�y@.�R@.E�@.$�@.@-��@-`B@-�@-V@,�@,��@,I�@,9X@+��@+t�@*�H@*��@*M�@*=q@)�@)�^@)��@)hs@)7L@(��@(�`@(Ĝ@(�9@(�u@(�@(bN@(1'@( �@(1'@(�u@(�u@(bN@(1'@(b@'�@'�;@'��@'�w@'��@'|�@'l�@';d@'+@&�y@&v�@%@%V@$��@$��@$Z@$Z@$I�@#��@#�F@#t�@#C�@"�@"��@"�\@"^5@"^5@"M�@"=q@"=q@"=q@"=q@"�@!�^@!x�@!G�@ �`@ Ĝ@ �u@ �@ r�@ Q�@��@l�@K�@�@��@��@�@��@�+@V@{@�T@�h@V@��@��@�D@�D@�D@��@I�@�@�@�
@dZ@"�@�@��@�\@=q@-@J@�#@��@x�@�@��@�`@��@�9@bN@  @��@�P@l�@K�@�@
=@�R@�+@V@@�T@@�h@/@��@�/@�/@�/@�/@�j@I�@(�@�@��@ƨ@�@S�@C�@33@33@33@"�@o@�@��@~�@=q@�@J@�#@��@�^@�7@X@7L@�@�@%@%@%@�`@Ĝ@�9@�@A�@A�@1'@ �@  @�@�;@��@|�@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B�BB\B{B�B!�B#�B#�B.B/B,B.B1'B49BD�BC�BE�BF�BL�Be`By�B�B�7B�VB�\B�bB�{B��B��B��B��B��B��B��B��B�{B|�Bo�Bv�B�DB�{B��B�3B�9B�FB�FB�wBɺB��B��B��BŢB�qB�^B�}B��BȴBÖBB��B�dB�B��B�BffB`BBs�BgmBN�B9XB-B"�B�BoB	7B��B�5B��B��BƨB�jB�B�B^5BK�B�BB
��B
�ZB
��B
�XB
��B
��B
��B
�bB
�B
|�B
q�B
gmB
[#B
I�B
A�B
;dB
49B
,B
#�B
�B
	7B	�B	�)B	�
B	��B	�XB	�B	��B	�1B	� B	k�B	aHB	]/B	VB	I�B	@�B	;dB	49B	.B	(�B	#�B	�B	�B	�B	uB	hB	\B	PB		7B	%B��B��B�B�BB��B��BƨB�wB�XB�?B�-B��B��B��B��B�oB�oB�hB�\B�=B�B�B�B~�B}�B}�B{�By�Bw�Bp�Bl�BdZBdZBbNB_;B]/B]/B[#BYBXBW
BS�BN�BM�BM�BG�BB�B=qB=qB=qB>wB;dB:^B8RB5?B33B33B33B1'B1'B0!B/B-B.B-B-B+B(�B)�B'�B'�B'�B&�B&�B$�B$�B$�B#�B#�B"�B"�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B"�B#�B$�B$�B%�B%�B%�B&�B&�B&�B%�B%�B&�B&�B&�B&�B'�B&�B%�B'�B(�B(�B(�B'�B(�B+B,B,B.B/B1'B2-B7LB<jB:^B9XB9XB9XB:^B;dB>wB?}BA�BC�BE�BI�BL�BL�BL�BN�BQ�BS�B[#BaHBcTBdZBdZBgmBhsBiyBm�Bp�Br�Bs�Bu�Bv�Bx�B|�B~�B� B�B�%B�+B�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�FB�FB�XB�jB�}BÖBŢBǮB��B��B��B��B�
B�B�B�#B�/B�HB�fB�sB�yB�B�B��B��B��B��B	  B	  B	B	JB	\B	hB	uB	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	$�B	)�B	-B	0!B	33B	6FB	:^B	>wB	?}B	E�B	H�B	I�B	J�B	K�B	L�B	N�B	R�B	W
B	ZB	\)B	`BB	aHB	e`B	jB	o�B	r�B	u�B	v�B	z�B	}�B	~�B	� B	�B	�B	�+B	�1B	�7B	�=B	�DB	�DB	�VB	�bB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�LB	�RB	�XB	�^B	�^B	�dB	�qB	�qB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
VB
PB
PB
JB
PB
PB
VB
VB
\B
\B
\B
bB
hB
hB
hB
\B
VB
PB
PB
PB
\B
bB
oB
uB
{B
oB
oB
bB
bB
\B
\B
\B
\B
bB
\B
\B
bB
hB
oB
uB
{B
uB
uB
uB
uB
uB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
#�B
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
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
)�B
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
/B
/B
/B
/B
/B
0!B
0!B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
@�B
@�B
@�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
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
L�B
L�B
N�B
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
Q�B
Q�B
Q�B
P�B
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
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
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
XB
YB
YB
YB
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
YB
ZB
ZB
ZB
ZB
ZB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
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
ffB
ffB
ffB
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
jB
jB
jB
jB
jB
jB
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B˒B˒BˬBˬB˒BˬB̳B��B�B(BFBqB!�B#�B#�B-�B/ B+�B-�B0�B4BDgBC{BE�BFtBL�BeFBy�B��B�B�"B�(B�.B�FB��B�~B��B��B��B��B��B��B�FB|�BoiBv�B�)B�aB��B��B�B�B�B�]BɆB��BҽB��B�mB�<B�*B�cBʌBȀB�aB�[B�oB�0B��B�xB��Bf2B`Bs�Bg8BN�B9$B,�B"�BB:B	B��B�BҽB͟B�tB�6B��B��B^BK�B~B�B
��B
�&B
ΥB
�$B
��B
��B
�gB
�.B
��B
|�B
qvB
g8B
Z�B
I�B
AUB
;0B
4B
+�B
#�B
eB
	B	�B	��B	��B	ʌB	�$B	��B	�_B	��B	�B	kQB	aB	\�B	U�B	I�B	@OB	;0B	4B	-�B	(�B	#�B	xB	YB	MB	@B	4B	(B	B		B	�B��B��B�KB�B��BбB�tB�BB�$B�B��B��B��B��B�eB�:B�:B�4B�(B�	B��B��B��B~�B}�B}�B{�By�Bw�BpoBlWBd&Bd&Ba�B_B\�B\�BZ�BX�BW�BV�BS�BN�BM�BM�BGzBB[B=<B="B=<B>BB;0B:*B8B5B2�B2�B2�B0�B0�B/�B.�B,�B-�B,�B,�B*�B(�B)�B'�B'�B'�B&�B&�B$�B$�B$�B#�B#�B"�B"�B �B vB �BjB�BdB~BxBxBWBWBqBpB�B�BpB vB �B vB"�B#�B$�B$�B%�B%�B%�B&�B&�B&�B%�B%�B&�B&�B&�B&�B'�B&�B%�B'�B(�B(�B(�B'�B(�B*�B+�B+�B-�B.�B0�B1�B7B<B:*B9	B9$B9$B:*B;0B>BB?HBA;BCaBESBI�BL~BL�BL�BN�BQ�BS�BZ�B`�Bc Bd&Bd&Bg8Bh>Bi*BmCBpUBraBshBu�Bv�Bx�B|�B~�B�B��B��B��B�"B�(B�B�FB�SB�EB�KB�xB��B�pB��B��B��B��B��B��B��B��B�B�B�B�$B�B�.B�aB�mB�_B˒BΊBбBөB��B��B��B��B��B��B�2B�$B�DB�KB�|B��B��B��B��B��B��B	�B	B	(B	B	@B	9B	?B	_B	QB	dB	 vB	!�B	"�B	"�B	$�B	)�B	,�B	/�B	2�B	6B	:*B	>BB	?.B	ESB	HfB	IlB	J�B	KxB	L~B	N�B	R�B	V�B	Y�B	[�B	`B	`�B	e,B	j0B	oOB	r|B	u�B	v�B	z�B	}�B	~�B	�B	��B	��B	��B	��B	�B	�	B	��B	��B	�"B	�.B	�:B	�:B	�&B	�&B	�FB	�MB	�YB	�QB	�~B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�*B	�0B	�"B	�<B	�.B	�OB	�;B	�[B	�GB	�gB	�gB	�SB	�_B	ȀB	�lB	ɆB	ʌB	ʌB	ʌB	̘B	͟B	ΊB	ϫB	ϫB	бB	бB	ѷB	ҽB	ҽB	��B	յB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�B	� B	�&B	�&B	�,B	�,B	�B	�>B	�DB	�KB	�0B	�KB	�6B	�QB	�WB	�]B	�]B	�CB	�cB	�oB	�[B	�aB	�nB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
 �B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B

�B
B
B
B
B
B
B
B
B
B
"B
B
B
(B
B
4B
B
B
B
"B
B
B
B
(B
B
:B
&B
FB
 B
 B
.B
.B
(B
(B
(B
B
B
B
(B
.B
B
:B
@B
,B
&B
&B
&B
@B
@B
 B
:B
:B
:B
&B
&B
FB
2B
2B
9B
?B
eB
eB
kB
kB
]B
qB
WB
]B
~B
~B
~B
pB
 vB
 �B
!|B
"�B
#�B
#�B
#�B
#�B
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
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
)�B
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
.�B
.�B
.�B
.�B
.�B
/�B
/�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
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
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
4B
3�B
4B
5B
5B
6B
6B
5�B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9$B
9	B
9$B
9$B
9$B
9$B
9$B
:*B
:B
:*B
:*B
:B
;0B
;B
<6B
<6B
<6B
<B
<6B
=<B
=<B
=<B
>BB
>(B
>BB
>(B
>BB
>BB
?.B
?HB
?.B
@4B
@OB
@OB
@4B
A;B
A;B
B[B
BAB
B[B
BAB
BAB
BAB
CaB
CGB
CGB
CaB
DgB
DgB
DMB
DgB
DMB
DMB
DMB
ESB
DgB
EmB
ESB
EmB
ESB
ESB
EmB
EmB
FtB
FtB
FYB
FtB
FtB
FtB
FtB
FYB
G_B
GzB
GzB
G_B
GzB
GzB
GzB
FYB
GzB
GzB
GzB
GzB
G_B
GzB
H�B
H�B
HfB
I�B
I�B
J�B
JrB
JrB
J�B
K�B
KxB
K�B
K�B
L�B
L�B
N�B
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
Q�B
Q�B
Q�B
P�B
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
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
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
W�B
X�B
X�B
X�B
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
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[�B
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
\�B
^B
^B
]�B
]�B
]�B
^B
]�B
]�B
_B
]�B
^�B
^�B
^�B
_B
_B
_B
^�B
_�B
`B
`B
`B
aB
aB
`�B
aB
aB
`�B
`�B
aB
bB
a�B
a�B
a�B
a�B
bB
bB
cB
cB
c B
cB
c B
cB
dB
dB
d&B
dB
dB
e,B
eB
e,B
fB
f2B
fB
g8B
g8B
h$B
h$B
h>B
h>B
h>B
h>B
i*B
i*B
iDB
j0B
jKB
j0B
j0B
jKB
jKB
kQ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.49(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806050048362018060500483620180605004836201806060037352018060600373520180606003735JA  ARFMdecpA19c                                                                20180531093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180531003510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180531003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180531003511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180531003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180531003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180531003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180531003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180531003512  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180531003512                      G�O�G�O�G�O�                JA  ARUP                                                                        20180531005548                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180531153719  CV  JULD            G�O�G�O�F�5�                JM  ARCAJMQC2.0                                                                 20180604154836  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180604154836  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180605153735  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                