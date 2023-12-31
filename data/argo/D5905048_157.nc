CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-07T00:35:18Z creation;2017-09-07T00:35:21Z conversion to V3.1;2019-12-19T07:58:11Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20170907003518  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_157                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�$6)�e�1   @�$6��� @4M!�.H��d����$1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @*=q@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BOp�BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�
D"w
D"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�A�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�Dҁ�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A��A◍A�v�A�E�A�A�/A�v�A� �A߅A��A�1A���A��HA���A޲-AޓuAޅA�z�A�p�A�ZA�  A�z�AܬA���A�;dA���A�5?A���A�`BA��;A���A��
A�n�A�$�A�1'A˾wA�M�AȁAǾwA���AƶFA�E�AżjAēuAÓuA�\)A��A�VA�ƨA��wA�O�A�1A���A��yA�VA�5?A�VA���A�/A�\)A��A�x�A�~�A��#A��\A���A��A�dZA��A���A�VA�1A�(�A�VA�7LA�M�A���A�1A��jA���A��A�\)A�7LA�jA�x�A�ZA�A�bA��RA��A��\A�hsA�A��A��A�M�A��A�t�A���A���A�ZA�%A��A�1A�r�A�XA�bNA�Q�A�-A��!A���A�  A�bA�|�A��A�+A�A;dA|^5Ay�FAx^5AwdZAv�AvI�Au�AshsArM�AqC�Am��Ajv�Ah�Ae�^Act�Ab �Aa\)AaA`VA_A^��A\�A[oAX�HAW`BAV�AT(�AR��AQ�wAP�AO��AO�AO?}AM��AJ��AJ=qAIp�AH1AF��AE�TADr�A@ZA>�\A=�mA=7LA<��A;C�A:A�A8ĜA7��A6$�A1�A0VA/�A/
=A.�RA-�TA,bNA*��A)�A)A(E�A'O�A&VA$�yA#VA"Q�AdZA��A��AZA�A��A��A33A^5A1A�+A�/A��A-A�A=qA�TA��A��AZA��AVA&�A�\A$�Ap�A"�A
��A
{A	A	O�A��A�jA{AG�AA�A��A�9A��AdZA�A�AM�A?}A ��A   @�@��D@�\)@�{@�bN@�V@�Z@��@�o@�ff@�{@�&�@�33@�hs@��y@�I�@��@�P@�!@�{@���@�$�@ݺ^@��/@�|�@���@ج@�(�@׮@�C�@�ȴ@��@���@��;@�+@�v�@с@���@϶F@ΰ!@��@�V@�ƨ@�@ȃ@��m@ǍP@�+@���@��@�bN@�
=@���@���@�Q�@�"�@��@�M�@��@�p�@�Q�@�@��\@�^5@�J@�%@���@�A�@�I�@�z�@��@�v�@�-@��@�p�@��9@��@�(�@��@�l�@�33@�o@�ff@�@���@�Ĝ@��@�1@��;@��@��@���@�hs@�O�@�7L@�Ĝ@�1@���@�C�@�+@�@���@�E�@���@�@��@���@�(�@��@��;@���@��@�S�@�;d@�33@���@�ȴ@���@��!@��\@�M�@�5?@�J@��@��#@��-@��h@�x�@��@�bN@� �@�  @��
@�C�@��y@���@���@��R@���@���@�~�@�{@���@��-@���@�`B@�V@��D@�9X@�  @��;@�ƨ@���@�l�@�@���@�ff@�@���@��^@��@�7L@��/@���@�Q�@���@���@��@���@�C�@��H@�ȴ@���@���@�@��7@��@���@�Z@�b@��@���@���@��P@��@�v�@���@��-@��@�`B@�`B@�`B@�?}@��/@��@��@�l�@�
=@���@��H@���@���@�^5@���@���@�O�@��@���@�r�@�9X@���@��;@���@��P@�l�@�C�@�
=@��H@���@��@���@���@�=q@�{@�{@�J@��#@��-@�p�@�/@��`@��j@�r�@��;@���@��P@�K�@��@�
=@��@��y@��@�ȴ@�n�@�{@��T@���@���@��h@�x�@�O�@���@���@��@�I�@�b@�ƨ@��P@�+@��H@��@���@�ȴ@���@��!@�~�@�ff@�E�@�-@��#@��@�X@�X@�?}@�V@���@��@�r�@�I�@�b@���@��;@���@��P@�dZ@�+@��@���@��R@��+@�M�@�5?@�-@��@�{@�J@���@��@��-@�`B@�7L@��@���@���@��@�Z@�  @+@~v�@}p�@}?}@|�/@|I�@|�@{ƨ@{�@{o@zM�@y��@y�^@y�^@y�^@y�^@y��@yX@x�9@w�@w�@v��@vV@v@u�T@u?}@t�@t�@t�@t1@s��@sƨ@so@r��@r~�@r�@q�7@q�@p��@pb@o�P@oK�@o
=@n�@nV@m��@m�@m�@m�@l��@l9X@k��@kt�@k33@jn�@j�@i�#@i&�@hr�@h1'@h1'@h  @g�@g��@g\)@g�@f�y@f�@fȴ@f��@f5?@e�-@e�@d�@c�F@b�H@b��@b�\@bn�@b�@a�@a��@a��@aX@`��@`�u@`b@_��@_�@_\)@^�y@^�R@^�R@^��@^E�@^@]�@]��@\��@\�/@\��@\�@\��@\Z@[�m@[�@["�@Z��@Y��@Y��@Y7L@X�9@XA�@W�w@Wl�@W�@Vv�@VE�@V@U�@U/@T�/@Tz�@S��@Sƨ@S�F@So@R��@R~�@R�@Q�#@Q��@Q��@Qhs@Q7L@P��@PA�@O��@Ol�@O
=@N�@N��@Nv�@N@M�h@M/@L��@LZ@K��@K�
@Kƨ@K�F@K�F@K��@K�@KS�@K@J�H@J��@Jn�@J�@I�#@Ix�@I7L@I7L@I�@H��@G�;@G\)@G�@F�R@Fff@F$�@F@E�@E@E��@EO�@E�@D��@D�@D�/@D��@D�j@D�@D�@D�D@DZ@DI�@DI�@C��@Ct�@CS�@Co@B^5@BJ@A�#@A�7@A�@@��@@Ĝ@@�9@@��@@Q�@@ �@@  @?�@?�@?�;@?��@?��@?|�@?\)@?
=@>�@>�R@>ff@=�@=�h@=`B@=�@=V@<��@<�/@<�j@<�j@<�@<�D@<9X@<�@;��@;ƨ@;S�@:�H@:�!@:^5@:J@9�#@9hs@97L@8�`@8�u@8r�@8bN@81'@8  @7�@7�;@7�w@7l�@7+@6��@6�@6ȴ@6�R@6ff@6E�@6$�@5�T@5�T@5@5�-@5�@5�@5`B@5�@4��@4(�@3�F@3t�@333@3@2M�@1��@1�^@1��@1x�@1X@17L@1&�@0��@0Ĝ@01'@0  @/�;@/�w@/l�@/;d@/�@.�R@.5?@.$�@.$�@.{@-��@-�-@-��@-�@-`B@-/@,�/@,�D@,Z@,9X@+�
@+��@+dZ@+C�@+@*�H@*�!@*~�@*M�@*-@*J@)�@)��@)&�@(�9@(A�@(  @'�@'�;@'�w@'l�@'+@&�@&�R@&��@&�+@&E�@%��@%?}@%/@%V@$�@$��@$9X@$1@$1@#�
@#��@#��@#dZ@#33@#33@#o@"�@"�!@"-@!�#@!�7@!x�@!hs@!X@!G�@!&�@ ��@ ��@ �u@ A�@ b@   @�w@�@\)@�y@ȴ@�R@�+@E�@{@��@@��@��@�h@`B@�@�@�@��@�@�@�D@j@9X@(�@�@ƨ@t�@33@��@��@��@n�@^5@J@�7@X@7L@�@��@�`@�`@Ĝ@�@bN@A�@A�@�@l�@+@�y@ȴ@��@v�@$�@�h@�@p�@p�@�@�@��@�D@I�@(�@��@S�@"�@o@�H@��@~�@n�@�@��@�^@�^@��@�7@hs@hs@G�@G�@G�@&�@Ĝ@�@A�@ �@b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A��A◍A�v�A�E�A�A�/A�v�A� �A߅A��A�1A���A��HA���A޲-AޓuAޅA�z�A�p�A�ZA�  A�z�AܬA���A�;dA���A�5?A���A�`BA��;A���A��
A�n�A�$�A�1'A˾wA�M�AȁAǾwA���AƶFA�E�AżjAēuAÓuA�\)A��A�VA�ƨA��wA�O�A�1A���A��yA�VA�5?A�VA���A�/A�\)A��A�x�A�~�A��#A��\A���A��A�dZA��A���A�VA�1A�(�A�VA�7LA�M�A���A�1A��jA���A��A�\)A�7LA�jA�x�A�ZA�A�bA��RA��A��\A�hsA�A��A��A�M�A��A�t�A���A���A�ZA�%A��A�1A�r�A�XA�bNA�Q�A�-A��!A���A�  A�bA�|�A��A�+A�A;dA|^5Ay�FAx^5AwdZAv�AvI�Au�AshsArM�AqC�Am��Ajv�Ah�Ae�^Act�Ab �Aa\)AaA`VA_A^��A\�A[oAX�HAW`BAV�AT(�AR��AQ�wAP�AO��AO�AO?}AM��AJ��AJ=qAIp�AH1AF��AE�TADr�A@ZA>�\A=�mA=7LA<��A;C�A:A�A8ĜA7��A6$�A1�A0VA/�A/
=A.�RA-�TA,bNA*��A)�A)A(E�A'O�A&VA$�yA#VA"Q�AdZA��A��AZA�A��A��A33A^5A1A�+A�/A��A-A�A=qA�TA��A��AZA��AVA&�A�\A$�Ap�A"�A
��A
{A	A	O�A��A�jA{AG�AA�A��A�9A��AdZA�A�AM�A?}A ��A   @�@��D@�\)@�{@�bN@�V@�Z@��@�o@�ff@�{@�&�@�33@�hs@��y@�I�@��@�P@�!@�{@���@�$�@ݺ^@��/@�|�@���@ج@�(�@׮@�C�@�ȴ@��@���@��;@�+@�v�@с@���@϶F@ΰ!@��@�V@�ƨ@�@ȃ@��m@ǍP@�+@���@��@�bN@�
=@���@���@�Q�@�"�@��@�M�@��@�p�@�Q�@�@��\@�^5@�J@�%@���@�A�@�I�@�z�@��@�v�@�-@��@�p�@��9@��@�(�@��@�l�@�33@�o@�ff@�@���@�Ĝ@��@�1@��;@��@��@���@�hs@�O�@�7L@�Ĝ@�1@���@�C�@�+@�@���@�E�@���@�@��@���@�(�@��@��;@���@��@�S�@�;d@�33@���@�ȴ@���@��!@��\@�M�@�5?@�J@��@��#@��-@��h@�x�@��@�bN@� �@�  @��
@�C�@��y@���@���@��R@���@���@�~�@�{@���@��-@���@�`B@�V@��D@�9X@�  @��;@�ƨ@���@�l�@�@���@�ff@�@���@��^@��@�7L@��/@���@�Q�@���@���@��@���@�C�@��H@�ȴ@���@���@�@��7@��@���@�Z@�b@��@���@���@��P@��@�v�@���@��-@��@�`B@�`B@�`B@�?}@��/@��@��@�l�@�
=@���@��H@���@���@�^5@���@���@�O�@��@���@�r�@�9X@���@��;@���@��P@�l�@�C�@�
=@��H@���@��@���@���@�=q@�{@�{@�J@��#@��-@�p�@�/@��`@��j@�r�@��;@���@��P@�K�@��@�
=@��@��y@��@�ȴ@�n�@�{@��T@���@���@��h@�x�@�O�@���@���@��@�I�@�b@�ƨ@��P@�+@��H@��@���@�ȴ@���@��!@�~�@�ff@�E�@�-@��#@��@�X@�X@�?}@�V@���@��@�r�@�I�@�b@���@��;@���@��P@�dZ@�+@��@���@��R@��+@�M�@�5?@�-@��@�{@�J@���@��@��-@�`B@�7L@��@���@���@��@�Z@�  @+@~v�@}p�@}?}@|�/@|I�@|�@{ƨ@{�@{o@zM�@y��@y�^@y�^@y�^@y�^@y��@yX@x�9@w�@w�@v��@vV@v@u�T@u?}@t�@t�@t�@t1@s��@sƨ@so@r��@r~�@r�@q�7@q�@p��@pb@o�P@oK�@o
=@n�@nV@m��@m�@m�@m�@l��@l9X@k��@kt�@k33@jn�@j�@i�#@i&�@hr�@h1'@h1'@h  @g�@g��@g\)@g�@f�y@f�@fȴ@f��@f5?@e�-@e�@d�@c�F@b�H@b��@b�\@bn�@b�@a�@a��@a��@aX@`��@`�u@`b@_��@_�@_\)@^�y@^�R@^�R@^��@^E�@^@]�@]��@\��@\�/@\��@\�@\��@\Z@[�m@[�@["�@Z��@Y��@Y��@Y7L@X�9@XA�@W�w@Wl�@W�@Vv�@VE�@V@U�@U/@T�/@Tz�@S��@Sƨ@S�F@So@R��@R~�@R�@Q�#@Q��@Q��@Qhs@Q7L@P��@PA�@O��@Ol�@O
=@N�@N��@Nv�@N@M�h@M/@L��@LZ@K��@K�
@Kƨ@K�F@K�F@K��@K�@KS�@K@J�H@J��@Jn�@J�@I�#@Ix�@I7L@I7L@I�@H��@G�;@G\)@G�@F�R@Fff@F$�@F@E�@E@E��@EO�@E�@D��@D�@D�/@D��@D�j@D�@D�@D�D@DZ@DI�@DI�@C��@Ct�@CS�@Co@B^5@BJ@A�#@A�7@A�@@��@@Ĝ@@�9@@��@@Q�@@ �@@  @?�@?�@?�;@?��@?��@?|�@?\)@?
=@>�@>�R@>ff@=�@=�h@=`B@=�@=V@<��@<�/@<�j@<�j@<�@<�D@<9X@<�@;��@;ƨ@;S�@:�H@:�!@:^5@:J@9�#@9hs@97L@8�`@8�u@8r�@8bN@81'@8  @7�@7�;@7�w@7l�@7+@6��@6�@6ȴ@6�R@6ff@6E�@6$�@5�T@5�T@5@5�-@5�@5�@5`B@5�@4��@4(�@3�F@3t�@333@3@2M�@1��@1�^@1��@1x�@1X@17L@1&�@0��@0Ĝ@01'@0  @/�;@/�w@/l�@/;d@/�@.�R@.5?@.$�@.$�@.{@-��@-�-@-��@-�@-`B@-/@,�/@,�D@,Z@,9X@+�
@+��@+dZ@+C�@+@*�H@*�!@*~�@*M�@*-@*J@)�@)��@)&�@(�9@(A�@(  @'�@'�;@'�w@'l�@'+@&�@&�R@&��@&�+@&E�@%��@%?}@%/@%V@$�@$��@$9X@$1@$1@#�
@#��@#��@#dZ@#33@#33@#o@"�@"�!@"-@!�#@!�7@!x�@!hs@!X@!G�@!&�@ ��@ ��@ �u@ A�@ b@   @�w@�@\)@�y@ȴ@�R@�+@E�@{@��@@��@��@�h@`B@�@�@�@��@�@�@�D@j@9X@(�@�@ƨ@t�@33@��@��@��@n�@^5@J@�7@X@7L@�@��@�`@�`@Ĝ@�@bN@A�@A�@�@l�@+@�y@ȴ@��@v�@$�@�h@�@p�@p�@�@�@��@�D@I�@(�@��@S�@"�@o@�H@��@~�@n�@�@��@�^@�^@��@�7@hs@hs@G�@G�@G�@&�@Ĝ@�@A�@ �@b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B�hB�=Bz�BgmB_;BXBW
B^5BhsBiyBiyBiyBhsBhsBhsBgmBe`B_;BVBS�BW
B��B��B�hB��BɺB�oB�B��B�5B�B�yB�;B�;B��B�FB��B�B��B�)B�TB��BDB1B��B��B�B	7BB�B  BB��B�BB{B�BA�BZBW
B`BB[#BO�BN�BL�BN�BW
BffBe`Br�BgmBM�B?}B?}BB�BC�BD�B?}B5?B7LB6FB1'B2-B:^BP�BdZB^5BJ�B33B�BB��B�`B�BÖB�?B��B�B|�Bw�Bo�BaHBO�B33B�BB
�B
�;B
�dB
�uB
{�B
r�B
^5B
D�B
&�B
!�B
�B
�B
�B
�B
oB
PB
B	��B	��B	�;B	ƨB	�RB	�B	��B	��B	��B	�oB	�JB	�B	y�B	l�B	`BB	P�B	H�B	B�B	49B	-B	.B	(�B	�B	!�B	�B	VB��B	1B	B��B��B��B�B�B��B�B��B��BǮBƨB�}B�qB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�=B�JB~�B�JB�PB�=B�oB�oB�bB�JB�1B�+B}�B{�B{�Bt�Bs�By�B|�B}�B{�B�1B�+B�B�B�B�%B�B�B�B� B�B~�B~�B~�Bz�Bv�By�Bz�By�Bx�B}�B|�B}�B|�By�B}�Bx�Bs�Bt�Bt�Bs�Bo�Bq�Bp�Bv�By�Bw�Bv�Bv�Bt�Bp�BcTB^5Bl�Bo�Bs�Bt�Bo�Bn�Bt�Br�Bn�Bn�Bo�Bs�Bt�Bw�B}�B}�B|�Bz�B}�B� B� B~�B|�B~�B�B�B�B�B�B�PB�bB�\B�PB�oB��B��B��B��B��B��B�B�B�9B�9B�?B�jB��B��B��B��B��B�B�B�HB�NB�HB�B�B�B�B��B��B��B��B	B	B	+B	1B	PB	\B	{B	�B	�B	�B	�B	&�B	(�B	)�B	)�B	(�B	)�B	0!B	5?B	:^B	:^B	<jB	@�B	D�B	G�B	J�B	P�B	Q�B	W
B	XB	ZB	\)B	]/B	`BB	bNB	cTB	dZB	ffB	hsB	hsB	jB	m�B	o�B	q�B	r�B	r�B	r�B	q�B	q�B	v�B	� B	�B	�B	�B	�7B	�PB	�\B	�\B	�\B	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�LB	�RB	�XB	�^B	�jB	�wB	��B	��B	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�HB	�TB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
%B
%B
+B
+B
+B
1B
	7B
1B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
PB
VB
\B
VB
\B
\B
\B
\B
VB
\B
\B
bB
hB
oB
oB
oB
hB
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
"�B
"�B
!�B
 �B
!�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
#�B
%�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
'�B
&�B
(�B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
-B
-B
.B
/B
/B
/B
.B
.B
.B
.B
.B
.B
/B
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
49B
5?B
5?B
5?B
6FB
6FB
5?B
6FB
8RB
7LB
8RB
8RB
9XB
9XB
9XB
8RB
8RB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
?}B
?}B
?}B
?}B
@�B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
?}B
A�B
B�B
B�B
C�B
C�B
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
E�B
E�B
F�B
E�B
D�B
F�B
F�B
E�B
D�B
F�B
G�B
G�B
G�B
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
J�B
J�B
J�B
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
N�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
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
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
T�B
S�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
S�B
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
XB
W
B
W
B
VB
XB
XB
XB
XB
XB
YB
XB
XB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
[#B
\)B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
_;B
`BB
`BB
aHB
aHB
aHB
aHB
`BB
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
cTB
dZB
e`B
dZB
dZB
dZB
dZB
dZB
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
jB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
iyB
jB
jB
jB
l�B
l�B
k�B
l�B
k�B
k�B
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
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
p�B
q�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
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
t�B
t�B
s�B
t�B
t�B
u�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�5B�B��B��B��B��B�B|jBh�B`BY1BW�B^jBh�Bi�Bi�Bi�Bh�Bh�Bh�Bg�Be�B`BBW�BV�B\�B��B��B��BңB�jB�B�GB�fB�;B�B��B�hB�4B��B�8B�B�mB�9BݲB�B��B�B	RB�B��B�B
rB�B��B�B�B��B��B{BB BBuB^BY�Bc�B]�BQ�BPHBM�BO�BW�BgmBf�Bt�Bi�BO�BAoBABC�BDMBEmBA�B8�B9�B88B3MB4TB;BP�Be�B`�BMjB6B"�B�B��B�B�B�%B��B��B�B}�Bx�BqABc:BR�B7LB_BB
�!B
�4B
��B
�B
}�B
tnB
aB
I�B
,qB
%,B
~B

B
�B
?B
uB
�B
tB	��B	��B	�TB	ʌB	�B	�GB	�ZB	�)B	�sB	�B	�6B	�%B	{�B	oB	b�B	S�B	J�B	DgB	6�B	/ B	/OB	*KB	 �B	"B	�B	}B	B		7B	aB��B��B�lB�!B�~B��B��B� B��BɆB�KB��B�.B�'B�qB�@B��B��B��B�TB��B�kB�)B��B��B�B�+B��B��B��B�[B�jB��B��B��B��B��B�B�RB�B�B~B}�Bv�BuZBz�B}�B~�B}qB��B�fB�B��B��B��B��B��B��B��B��B�B�B�B|BxB{JB{�B{By�B~�B}�B~wB}�B{JB~�BzDBu%Bu�Bu�Bt�Bp�BsBq�BwLBzxBxRBwLBw�Bv`Br�BgB_�Bl�Bp!BtnBu�BqABo�Bu%Bs�Bo�Bo�Bp�Bt9Bu%Bx8B~wB~�B}�B{�B~�B��B��B�B}�B�B��B��B�B�gB�B��B��B��B�VB�&B�B��B��B�NB�`B��B�OB��B��B��B�+B�VB��B�B�BB͟B�TB�SB�QB�bB��B�NB��B��B� B�'B�B�$B�8B�B	[B	SB	�B	�B	�B	B	�B	�B	�B	kB	 vB	'B	)*B	*B	*0B	)_B	*B	0oB	5�B	:xB	:�B	<�B	@�B	D�B	G�B	KDB	QB	RTB	W$B	XEB	ZQB	\CB	]~B	`\B	b�B	c�B	d�B	f�B	h�B	h�B	j�B	m�B	o�B	q�B	r�B	r�B	r�B	q�B	rB	wLB	�B	�-B	�GB	�{B	��B	�jB	�vB	�vB	�\B	�pB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�DB	�0B	�QB	�cB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	żB	��B	��B	��B	�	B	�B	��B	�B	�PB	�<B	�B	�(B	�(B	��B	�B	�,B	�oB	�{B	�MB	�YB	�1B	�7B	�B	�7B	�KB	׍B	֡B	�]B	�dB	�~B	�VB	�\B	�\B	�|B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�8B	�LB	�B	�B	�6B	�B	�(B	�B	�B	�.B	�B	�]B	�BB	�B
 B
 B
;B
'B
;B
UB
;B
'B
AB
AB
AB
GB
{B
MB
%B
%B
?B
?B
?B
YB
?B
?B
_B
tB
YB
EB
_B
EB
�B
	�B
�B

XB

XB

�B
xB
^B
xB
^B
xB
xB
~B
~B
dB
�B
�B
�B
\B
pB
vB
vB
vB
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
!B
!�B
#B
#B
"B
"�B
# B
# B
#B
#�B
%�B
$�B
$�B
%�B
%�B
'B
'B
'B
'B
'B
&B
&B
&2B
%FB
(>B
'RB
)B
*B
*B
)B
*0B
*0B
*B
*B
*KB
*0B
*KB
,"B
,=B
,"B
,WB
-)B
-B
-)B
-)B
-)B
./B
-CB
-CB
./B
/B
/5B
/5B
./B
.IB
.IB
.IB
.IB
.IB
/iB
0oB
0UB
1vB
1[B
1[B
2GB
2aB
3MB
3MB
3hB
4nB
5ZB
5tB
5tB
6zB
6zB
5tB
6�B
8lB
7�B
8�B
8lB
9rB
9�B
9�B
8�B
8�B
9�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
@�B
@�B
@�B
?�B
?�B
?�B
?�B
@�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
@�B
?�B
A�B
B�B
B�B
C�B
C�B
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
E�B
E�B
F�B
E�B
D�B
F�B
F�B
E�B
D�B
F�B
G�B
G�B
G�B
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
J�B
J�B
J�B
L�B
L�B
M�B
M�B
NB
M�B
M�B
M�B
M�B
NB
N�B
NB
M�B
NB
NB
N�B
N�B
N�B
PB
P.B
QB
QB
Q B
RB
R B
RB
RB
Q�B
RB
RB
R B
RB
S&B
S&B
TB
SB
SB
TB
T,B
TB
T�B
TB
TB
TB
T�B
TB
S&B
S&B
S&B
T,B
UB
U2B
U2B
TFB
VB
W
B
W$B
W$B
W$B
X+B
X+B
W$B
W$B
VSB
XEB
XEB
X+B
X+B
X+B
Y1B
XEB
XEB
ZB
ZB
Z7B
ZQB
Z7B
[#B
[=B
ZQB
Z7B
Z7B
Z7B
[=B
\]B
[WB
\CB
\CB
\CB
\CB
]IB
]IB
]IB
]IB
^5B
]IB
]IB
]IB
]dB
^jB
^jB
_pB
`BB
`vB
`\B
_VB
`\B
`\B
abB
a|B
abB
abB
`�B
a|B
bhB
bhB
b�B
bhB
bhB
cnB
cnB
cnB
cnB
cnB
cnB
dtB
ezB
dtB
dtB
dtB
d�B
dtB
ezB
f�B
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
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
i�B
iyB
i�B
iyB
i�B
i�B
i�B
jB
i�B
jB
j�B
j�B
j�B
j�B
j�B
jB
j�B
i�B
j�B
j�B
j�B
l�B
l�B
k�B
l�B
k�B
k�B
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
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
p�B
q�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
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
t�B
t�B
tB
t�B
t�B
u�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709110036272017091100362720170911003627201806221318482018062213184820180622131848201804050721122018040507211220180405072112  JA  ARFMdecpA19c                                                                20170907093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170907003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170907003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170907003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170907003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170907003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170907003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170907003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170907003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170907003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20170907005547                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170907153258  CV  JULD            G�O�G�O�F�!�                JM  ARCAJMQC2.0                                                                 20170910153627  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170910153627  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222112  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041848  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                