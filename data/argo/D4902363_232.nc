CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-23T00:35:27Z creation;2018-04-23T00:35:37Z conversion to V3.1;2019-12-19T07:44:07Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180423003527  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_232                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�]5��� 1   @�]6O���@:�W����dk4�J�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�3D��3D�fD�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @7
=@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9�)C;��C=��C?�)CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,��D,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�
DZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D��D�A�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D��D�A�D��D���D�D�(R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�=qA��A�{A�A��A��A��yA��HA��jA���A�$�A�1A�%A���A��jA� �A�v�A�-A���A���A��9A�^5A��uA�I�A��HA�ĜA���A�E�A�A��FA�l�A�M�A�"�A���A�VA��A��A���A��TA��9A��A�hsA�K�A�-A�VA��A��/A���A�1'A���A�~�A���A�;dA�ȴA��9A��mA�~�A�9XA���A���A�A�ƨA�$�A��A���A��hA��A��RA�(�A���A�E�A��yA�^5A��
A�x�A���A���A�7LA�A��uA��7A��FA��A�  A��A��RA���A�z�A��+A�A��A�K�A��A���A~ĜA}�A|I�A{hsA{?}AzQ�Ax��Ax��Aw��Av�jAv{AuAs;dAr�jArI�Aq�FAqApbNAoAoS�An��An�\Al��Ak��Aj�RAidZAioAh �AfȴAeXAdr�AchsAbffA`n�A]�mA[�AZ�!AY�AXĜAW��AV�9AU33ATZAS33AP��AOhsAN�`AM/AK�;AK;dAJ��AJffAI+AG"�AE�AEG�AD�AD�AB��ABbAAdZA@�uA?�wA?S�A>�HA>v�A>ȴA>ĜA>1A<  A;VA9�A8�9A8Q�A8^5A8�+A8ZA7��A7XA7A6VA5��A533A4�/A4�A3�;A3G�A2ĜA1��A0�DA/`BA.r�A-�TA-C�A,VA+�A)�^A(�!A(VA'�A'|�A'A&�A#�TA"�yA"�jA!|�A+A/AI�A1'A{A�;At�A�An�A��A�A��AdZA��A`BA�uA�A`BA�jAƨA1A$�A
��A	l�A	?}A��A��A=qA�A?}A5?A?}A�RA�Av�A�TA��A;dA�/A9XA�hAC�A �A V@�C�@�{@���@���@��\@��T@�/@��;@�V@��^@��7@��@���@�
=@�^5@��@���@�+@��@�j@�A�@�;d@���@�V@�(�@���@�!@�5?@�7L@�@��#@�"�@�Z@�1'@��@�$�@�X@�z�@��@�l�@���@�n�@�J@�p�@��@ЋD@�(�@·+@�G�@�  @���@�E�@��@ț�@�C�@ư!@�&�@�bN@+@�`B@�A�@��w@�v�@�`B@��@�t�@�n�@�hs@�(�@�33@���@�?}@��9@��;@�33@�~�@���@���@�hs@�O�@��@���@��@��@�dZ@�S�@�+@�@���@�ff@�J@�p�@� �@�C�@�n�@��@�%@�1'@���@�"�@���@��\@�~�@�~�@�v�@�ff@�E�@�-@�{@��@�@��-@���@�`B@���@��m@���@��h@���@�b@���@�+@��@�=q@��h@���@�(�@��w@�;d@�J@���@�j@��F@�+@��R@���@��@��@�A�@�dZ@��@��!@��@��^@��@��u@�+@�v�@�-@��@��#@���@���@�&�@�z�@�(�@��;@�|�@�dZ@�33@��\@�J@��@��-@�hs@���@�ƨ@�|�@���@���@�-@��@���@��@�x�@�X@�?}@�7L@��`@�r�@�1'@���@���@�t�@�\)@�K�@�;d@�"�@�
=@���@��@�ȴ@���@�V@�@��#@�@��-@���@���@�`B@���@�@}�@}�@}��@}��@}p�@|��@|��@|z�@|(�@{��@{S�@z�!@z-@y�@yx�@yhs@yG�@y%@x�9@xA�@w��@wl�@w|�@w|�@w+@w
=@v�@vE�@u�-@u�-@u�h@u�@t��@tj@s��@s33@s33@s"�@r��@q�#@q7L@p�`@p�9@p  @o+@n��@nE�@m@m�@m`B@l��@k��@j�H@j��@jn�@jJ@i��@i��@jJ@jJ@jJ@jJ@j�@i��@i��@i��@iX@i�@hbN@g�w@g�@fff@fE�@f$�@f@f@f{@f$�@f$�@f$�@f$�@e�T@e��@e?}@d�/@d�D@dz�@dj@d9X@cƨ@ct�@cdZ@cS�@cC�@c@b�@b�H@b��@b��@b��@b�\@b^5@b=q@bJ@a��@a�@a�#@a�#@aX@_�w@_�@^�@^E�@]�-@\��@\z�@[��@["�@Z�@Z�H@Z��@Z��@Z��@Z�!@Z�\@ZM�@Y��@Y�@W�@W|�@W;d@V��@V�R@V�+@Vff@VV@VE�@V$�@U�@U�h@Up�@U?}@U�@T�@R�@R~�@Rn�@R^5@R-@Q�^@P�`@P��@P�@Pb@O��@O�@Ol�@N�@N��@N�+@N$�@M@Mp�@MO�@M�@MV@MV@L��@L�@L��@L�D@Lz�@LZ@L9X@L�@K�F@KC�@J��@I�@I�#@I�#@I�^@Ix�@I&�@HĜ@HbN@G��@G�P@G
=@F�y@F�R@F�+@F@E/@D�@D��@D�D@DI�@DI�@DI�@D9X@D�@Ct�@B~�@B=q@A�@A�#@A�^@Ahs@A&�@A&�@A�@@��@@��@@Ĝ@@  @?��@?�w@?�w@?�w@?�P@?l�@?K�@?+@?
=@>ȴ@>v�@>$�@=�T@=@=�@<��@<z�@<Z@<9X@;�m@;ƨ@;��@;33@;@:�@:��@:��@:��@:=q@9�@9�#@9��@9hs@97L@9%@8��@8�u@8 �@7�@7�@7�P@7|�@7+@6��@6v�@65?@5�@5��@5�h@4��@4z�@49X@4�@3��@3ƨ@3S�@3"�@2�H@2��@2�!@2n�@2�@1��@1�@1�#@1�^@1��@1x�@1%@0��@0bN@0b@/�;@/|�@.�y@.��@.5?@-��@-��@-`B@-�@,��@,I�@,9X@,�@+��@+�
@+ƨ@+C�@*�!@*=q@)�^@(Ĝ@(Q�@(Q�@(1'@(b@(b@'��@'|�@'�@&�y@&ȴ@&ff@&5?@%��@%p�@%/@$��@$��@$��@$�j@$z�@$�@$1@#�m@#�@#dZ@#@"J@!�@!��@!��@!G�@!7L@!&�@!�@!%@!�@!�@ ��@ �`@ ��@ Ĝ@ �@ Q�@   @�@|�@+@�@��@�y@�R@��@ff@5?@5?@$�@�T@?}@�@�j@�@�@j@Z@Z@I�@9X@(�@1@��@�
@dZ@S�@33@"�@"�@��@~�@=q@��@hs@7L@%@��@bN@1'@  @��@�@��@�P@l�@K�@;d@;d@+@�R@v�@$�@{@@�h@�@�@��@Z@��@��@S�@C�@o@��@�!@�\@^5@�@��@hs@G�@X@X@X@��@�9@�@1'@�P@\)@+@��@�@�R@v�@ff@E�@$�@@�T@��@�-@��@p�@V@�@��@��@j@I�@(�@�F@�@t�@C�@33@33@"�@o@
�!@
n�@
M�@
=q@
�@	�#@	�^@	�^@	��@	�7@	X@	7L@	&�@	%@��@��@��@Ĝ@�9@��@��@��@�@�@bN@ �@�@|�@\)@K�@;d@+@�@�@��@ff@ff@V@$�@�T@`B@/@��@�/@�@��@��@��@�D@�D@�D@�D@�D@�D@Z@I�@�@ƨ@�F@��@�@t�@C�@@�H@�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�=qA��A�{A�A��A��A��yA��HA��jA���A�$�A�1A�%A���A��jA� �A�v�A�-A���A���A��9A�^5A��uA�I�A��HA�ĜA���A�E�A�A��FA�l�A�M�A�"�A���A�VA��A��A���A��TA��9A��A�hsA�K�A�-A�VA��A��/A���A�1'A���A�~�A���A�;dA�ȴA��9A��mA�~�A�9XA���A���A�A�ƨA�$�A��A���A��hA��A��RA�(�A���A�E�A��yA�^5A��
A�x�A���A���A�7LA�A��uA��7A��FA��A�  A��A��RA���A�z�A��+A�A��A�K�A��A���A~ĜA}�A|I�A{hsA{?}AzQ�Ax��Ax��Aw��Av�jAv{AuAs;dAr�jArI�Aq�FAqApbNAoAoS�An��An�\Al��Ak��Aj�RAidZAioAh �AfȴAeXAdr�AchsAbffA`n�A]�mA[�AZ�!AY�AXĜAW��AV�9AU33ATZAS33AP��AOhsAN�`AM/AK�;AK;dAJ��AJffAI+AG"�AE�AEG�AD�AD�AB��ABbAAdZA@�uA?�wA?S�A>�HA>v�A>ȴA>ĜA>1A<  A;VA9�A8�9A8Q�A8^5A8�+A8ZA7��A7XA7A6VA5��A533A4�/A4�A3�;A3G�A2ĜA1��A0�DA/`BA.r�A-�TA-C�A,VA+�A)�^A(�!A(VA'�A'|�A'A&�A#�TA"�yA"�jA!|�A+A/AI�A1'A{A�;At�A�An�A��A�A��AdZA��A`BA�uA�A`BA�jAƨA1A$�A
��A	l�A	?}A��A��A=qA�A?}A5?A?}A�RA�Av�A�TA��A;dA�/A9XA�hAC�A �A V@�C�@�{@���@���@��\@��T@�/@��;@�V@��^@��7@��@���@�
=@�^5@��@���@�+@��@�j@�A�@�;d@���@�V@�(�@���@�!@�5?@�7L@�@��#@�"�@�Z@�1'@��@�$�@�X@�z�@��@�l�@���@�n�@�J@�p�@��@ЋD@�(�@·+@�G�@�  @���@�E�@��@ț�@�C�@ư!@�&�@�bN@+@�`B@�A�@��w@�v�@�`B@��@�t�@�n�@�hs@�(�@�33@���@�?}@��9@��;@�33@�~�@���@���@�hs@�O�@��@���@��@��@�dZ@�S�@�+@�@���@�ff@�J@�p�@� �@�C�@�n�@��@�%@�1'@���@�"�@���@��\@�~�@�~�@�v�@�ff@�E�@�-@�{@��@�@��-@���@�`B@���@��m@���@��h@���@�b@���@�+@��@�=q@��h@���@�(�@��w@�;d@�J@���@�j@��F@�+@��R@���@��@��@�A�@�dZ@��@��!@��@��^@��@��u@�+@�v�@�-@��@��#@���@���@�&�@�z�@�(�@��;@�|�@�dZ@�33@��\@�J@��@��-@�hs@���@�ƨ@�|�@���@���@�-@��@���@��@�x�@�X@�?}@�7L@��`@�r�@�1'@���@���@�t�@�\)@�K�@�;d@�"�@�
=@���@��@�ȴ@���@�V@�@��#@�@��-@���@���@�`B@���@�@}�@}�@}��@}��@}p�@|��@|��@|z�@|(�@{��@{S�@z�!@z-@y�@yx�@yhs@yG�@y%@x�9@xA�@w��@wl�@w|�@w|�@w+@w
=@v�@vE�@u�-@u�-@u�h@u�@t��@tj@s��@s33@s33@s"�@r��@q�#@q7L@p�`@p�9@p  @o+@n��@nE�@m@m�@m`B@l��@k��@j�H@j��@jn�@jJ@i��@i��@jJ@jJ@jJ@jJ@j�@i��@i��@i��@iX@i�@hbN@g�w@g�@fff@fE�@f$�@f@f@f{@f$�@f$�@f$�@f$�@e�T@e��@e?}@d�/@d�D@dz�@dj@d9X@cƨ@ct�@cdZ@cS�@cC�@c@b�@b�H@b��@b��@b��@b�\@b^5@b=q@bJ@a��@a�@a�#@a�#@aX@_�w@_�@^�@^E�@]�-@\��@\z�@[��@["�@Z�@Z�H@Z��@Z��@Z��@Z�!@Z�\@ZM�@Y��@Y�@W�@W|�@W;d@V��@V�R@V�+@Vff@VV@VE�@V$�@U�@U�h@Up�@U?}@U�@T�@R�@R~�@Rn�@R^5@R-@Q�^@P�`@P��@P�@Pb@O��@O�@Ol�@N�@N��@N�+@N$�@M@Mp�@MO�@M�@MV@MV@L��@L�@L��@L�D@Lz�@LZ@L9X@L�@K�F@KC�@J��@I�@I�#@I�#@I�^@Ix�@I&�@HĜ@HbN@G��@G�P@G
=@F�y@F�R@F�+@F@E/@D�@D��@D�D@DI�@DI�@DI�@D9X@D�@Ct�@B~�@B=q@A�@A�#@A�^@Ahs@A&�@A&�@A�@@��@@��@@Ĝ@@  @?��@?�w@?�w@?�w@?�P@?l�@?K�@?+@?
=@>ȴ@>v�@>$�@=�T@=@=�@<��@<z�@<Z@<9X@;�m@;ƨ@;��@;33@;@:�@:��@:��@:��@:=q@9�@9�#@9��@9hs@97L@9%@8��@8�u@8 �@7�@7�@7�P@7|�@7+@6��@6v�@65?@5�@5��@5�h@4��@4z�@49X@4�@3��@3ƨ@3S�@3"�@2�H@2��@2�!@2n�@2�@1��@1�@1�#@1�^@1��@1x�@1%@0��@0bN@0b@/�;@/|�@.�y@.��@.5?@-��@-��@-`B@-�@,��@,I�@,9X@,�@+��@+�
@+ƨ@+C�@*�!@*=q@)�^@(Ĝ@(Q�@(Q�@(1'@(b@(b@'��@'|�@'�@&�y@&ȴ@&ff@&5?@%��@%p�@%/@$��@$��@$��@$�j@$z�@$�@$1@#�m@#�@#dZ@#@"J@!�@!��@!��@!G�@!7L@!&�@!�@!%@!�@!�@ ��@ �`@ ��@ Ĝ@ �@ Q�@   @�@|�@+@�@��@�y@�R@��@ff@5?@5?@$�@�T@?}@�@�j@�@�@j@Z@Z@I�@9X@(�@1@��@�
@dZ@S�@33@"�@"�@��@~�@=q@��@hs@7L@%@��@bN@1'@  @��@�@��@�P@l�@K�@;d@;d@+@�R@v�@$�@{@@�h@�@�@��@Z@��@��@S�@C�@o@��@�!@�\@^5@�@��@hs@G�@X@X@X@��@�9@�@1'@�P@\)@+@��@�@�R@v�@ff@E�@$�@@�T@��@�-@��@p�@V@�@��@��@j@I�@(�@�F@�@t�@C�@33@33@"�@o@
�!@
n�@
M�@
=q@
�@	�#@	�^@	�^@	��@	�7@	X@	7L@	&�@	%@��@��@��@Ĝ@�9@��@��@��@�@�@bN@ �@�@|�@\)@K�@;d@+@�@�@��@ff@ff@V@$�@�T@`B@/@��@�/@�@��@��@��@�D@�D@�D@�D@�D@�D@Z@I�@�@ƨ@�F@��@�@t�@C�@@�H@�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�
B�B�B�B�B�
B��BȴB�XB�-BǮBɺBÖB�^B�B��B�?B�9B�RB�9B��B�uB��B��B��B��B��B�B��B��B��B��B�uB�JB��B�uB�B�dB�dB�dB�}B��B�wB�qB�dB�^B�FB��B��B��B�hB�%B�BaHBE�B+B��BƨB�-B��B�VBk�B}�Bt�Bs�BjBbNBYBI�BL�BD�B9XB0!B-B#�B%�B�BoB
��B
�#B
�FB
ŢB
�XB
�3B
��B
B
�RB
��B
�\B
�%B
{�B
jB
cTB
E�B
N�B
[#B
T�B
ZB
N�B
<jB
J�B
A�B
2-B
2-B
'�B
�B
%�B
"�B
�B
�B
�B
oB
bB
JB
B	�B	�`B	�B	�/B	�ZB	�B	��B	�LB	�'B	��B	�{B	� B	[#B	_;B	ffB	gmB	ffB	_;B	ZB	P�B	E�B	B�B	$�B	'�B	'�B	{B	bB	�B	hB	JB��B�mB�B��B��B�B�5B�HB�/B�
B��B��B��B��B�)B�
BƨB�B�?B�'B�3B�dBɺB��B�
B�B��B��B��BǮB��BɺBǮB��B�jB�LB��B��B��B�B�B��B��B��B�B�%B�bB�VB�=B�Bt�B\)Bs�Bw�B_;B;dB(�B\)Bq�Bp�Bk�BffBaHB\)BVBS�BP�B\)BQ�BC�BK�BQ�BJ�BE�B9XB&�B#�B49B-BG�BF�BB�B?}B9XB8RB+B/B33B=qB9XB49B49B33B1'B,B)�B.B.B$�B$�B$�B)�B%�B�B&�B$�B�B�B'�B+B&�B �B"�B#�B �B�B�B�B�B �B�B{B�B�B�B!�B�BuBhB��B��B��B�BuB�B�B�B �B �B�B"�B!�B�B�B�B�BuBhB�B�B�B �B�B�B�B{B�B�B�B �B&�B!�B"�B'�B+B)�B-B0!B5?B6FB?}BA�B@�BC�BE�BI�BL�BN�BO�BM�BK�BJ�BO�BS�BT�BS�BS�BP�BQ�BN�BK�BF�BK�BP�BT�BR�BVB^5BbNBgmBiyBl�Bl�Bl�Bk�Bk�Bl�Bk�Bk�Bk�Bk�BjBe`BaHB[#B\)BbNBiyBq�Bt�Bv�Bw�Bv�Bx�Bz�B� B�%B�B~�B�B�VB�bB�{B��B��B��B��B��B��B�B�B�B�9B�3B�B�B�qBǮB��B��B��B��B��B��B��B�B�B�#B�B�
B�)B�NB�NB�TB�ZB�fB�B��B��B��B	B	%B	
=B	JB	JB	PB	VB	JB	\B	�B	�B	#�B	&�B	(�B	+B	+B	,B	-B	.B	.B	/B	/B	0!B	33B	7LB	:^B	;dB	;dB	:^B	7LB	5?B	7LB	@�B	P�B	W
B	XB	YB	ZB	^5B	`BB	bNB	dZB	dZB	gmB	k�B	o�B	p�B	t�B	t�B	t�B	u�B	v�B	z�B	}�B	�B	�B	�B	�1B	�7B	�1B	�DB	�bB	�bB	�bB	�oB	�{B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�-B	�-B	�-B	�-B	�'B	�-B	�9B	�9B	�LB	�LB	�XB	�jB	�qB	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ǮB	ǮB	ƨB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�
B	��B	��B	�B	�)B	�#B	�)B	�/B	�HB	�HB	�ZB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�fB	�`B	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	7B
	7B
1B
+B
+B
+B
1B
1B

=B
DB
PB
PB
JB
DB
DB
bB
oB
hB
oB
{B
uB
oB
hB
VB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
!�B
"�B
"�B
"�B
"�B
!�B
"�B
$�B
#�B
#�B
$�B
$�B
$�B
$�B
#�B
&�B
&�B
'�B
'�B
%�B
%�B
'�B
(�B
(�B
(�B
(�B
&�B
+B
,B
-B
-B
-B
,B
.B
/B
0!B
0!B
/B
/B
1'B
1'B
1'B
1'B
1'B
0!B
/B
1'B
0!B
1'B
2-B
1'B
1'B
33B
33B
49B
5?B
6FB
6FB
5?B
7LB
9XB
9XB
9XB
9XB
8RB
6FB
6FB
8RB
8RB
7LB
;dB
>wB
>wB
>wB
>wB
=qB
=qB
=qB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
A�B
B�B
C�B
B�B
A�B
A�B
C�B
B�B
A�B
B�B
@�B
=qB
B�B
B�B
A�B
A�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
G�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
I�B
G�B
E�B
G�B
J�B
K�B
K�B
K�B
M�B
N�B
M�B
M�B
M�B
L�B
M�B
L�B
K�B
M�B
M�B
M�B
M�B
K�B
L�B
L�B
K�B
L�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
O�B
Q�B
Q�B
T�B
S�B
T�B
T�B
S�B
R�B
VB
T�B
VB
W
B
XB
W
B
XB
YB
YB
YB
YB
ZB
XB
\)B
\)B
\)B
[#B
YB
ZB
ZB
ZB
YB
]/B
]/B
^5B
_;B
^5B
^5B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
bNB
cTB
dZB
e`B
e`B
dZB
ffB
ffB
ffB
e`B
e`B
ffB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
gmB
hsB
hsB
gmB
gmB
ffB
e`B
dZB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
iyB
hsB
jB
jB
iyB
iyB
hsB
k�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
m�B
n�B
m�B
l�B
o�B
o�B
n�B
o�B
n�B
n�B
p�B
p�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�SB�?B�1B�7B�+B�B�$B� B�lB�B�BǮB��B��B�0B��B�kB��B��B�lB��B��B�B�`B��B�DB�fB��B�kB��B�bB�-B�IB��B��B��B��B�"B�JB��B��B��B��B��B��B��B��B��B�0B��B��B�B��B��BdBH1B�B��BɺB��B�,B�4Bn�B~�BvFBt�Bk�BcnBZkBK)BM�BE�B:�B1vB-�B%,B&�B�B&B
��B
�B
�^B
�+B
��B
�B
�[B
��B
�	B
�B
�4B
��B
}�B
l�B
e`B
H�B
P�B
[�B
VB
Z�B
P.B
>]B
KB
BuB
3�B
3B
)_B
�B
&�B
#nB
jB
sB
YB
&B
 B
B
�B	�B	�B	�B	��B	��B	�YB	�aB	�	B	�aB	�XB	�9B	�uB	^�B	a�B	g�B	h�B	g�B	`�B	[�B	R�B	G+B	DMB	'�B	)�B	(�B	�B	�B	SB	 B	B��B��B�=B�8B�tB��B��B�B�B�+B��BԯB՛B҉B��B׍B�B��B��B��B�B��BɺB��B�sBخB��B�{B̳BȚB�^B�=B�KB�oB�VB�8B��B��B�mB�"B��B��B�B��B�YB�zB��B�B��B�BvFB^�Bt�BxlBabB?B-CB\�Bq�Bp�BlBg8Ba�B]IBWYBU�BR B\�BS&BESBL�BR�BK�BF�B;B)yB&LB5�B.�BG�BG+BCGB@4B:^B9>B,�B0UB3�B=�B9�B5B4�B3�B1�B-B*�B.�B.�B%�B%�B%�B*B&�B7B'mB%�B�B�B(XB+kB'mB!�B#nB$tB!bB�B�BBWB!HBWB�BIB]BKB!�B/BFB:B��B��B��B��BBCB]BCB!-B!-B BB#:B"NB \B 'B 'BOB�BTByBqBIB!-B�B�B vB�BVB�B~B!�B'mB"�B#�B(�B+�B*�B-�B1B6+B72B?�BBBAUBD3BF?BJ=BMBOBPBN<BL0BKDBP.BTBUBT,BTFBQNBR:BOBBLdBG�BL�BQ�BU�BS�BV�B^�Bb�Bg�Bi�Bl�Bl�Bl�Bk�Bk�Bl�Bk�Bk�Bk�Bk�Bj�Be�Ba�B\B]/Bc:Bj0BrBu%BwLBx8BwLByrB{B��B��B��B�B��B��B��B��B�B�9B�)B�NB�ZB��B�QB�cB��B�nB��B��B�B��B��B�B��B�B�"B�dB�jB�FB�mB�SB�=B�kB׍BܒB�hB�B�B��B�B��B�B��B�jB	9B	tB	
XB	dB	�B	�B	pB	�B	�B	�B	B	$&B	'B	)B	+6B	+6B	,"B	-)B	./B	./B	/5B	/OB	0oB	3hB	7�B	:�B	;B	;B	:xB	7�B	5�B	8B	A B	QB	W
B	XEB	YKB	ZQB	^OB	`vB	b�B	dtB	d�B	g�B	k�B	o�B	p�B	t�B	t�B	t�B	u�B	wB	{0B	~(B	�B	�MB	�SB	�fB	�RB	��B	�xB	�bB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�2B	�>B	�B	�B	�
B	�2B	�FB	�>B	�B	�CB	�5B	�-B	�MB	�3B	�-B	�GB	�-B	�GB	�[B	�GB	�TB	��B	��B	��B	��B	��B	��B	ÖB	��B	��B	ȴB	��B	ȴB	ǮB	��B	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�,B	�B	�B	�$B	�$B	�$B	�B	�$B	�$B	�$B	�EB	�+B	�B	�KB	�+B	�$B	�@B	�bB	�KB	�CB	�qB	ܒB	�~B	�|B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B
 B
 B
 4B
 4B
 B
-B
-B
3B
B
-B
3B
B
MB
9B
3B
GB
MB
AB
[B
uB
MB
	RB
	7B
KB
EB
_B
zB
fB
�B

rB
xB
jB
jB
dB
�B
�B
}B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
!�B
"�B
"�B
#B
"�B
!�B
"�B
$�B
#�B
$B
$�B
$�B
$�B
$�B
$B
'B
'B
(
B
($B
&2B
&B
(
B
)B
)*B
)B
)B
'RB
+B
,"B
-)B
-)B
-)B
,=B
./B
/5B
0;B
0;B
/OB
/5B
1'B
1'B
1AB
1AB
1[B
0;B
/OB
1AB
0UB
1[B
2GB
1[B
1[B
3MB
3�B
4TB
5ZB
6`B
6`B
5�B
7fB
9XB
9rB
9rB
9�B
8lB
6zB
6�B
8�B
8�B
7�B
;B
>wB
>�B
>�B
>wB
=�B
=�B
=�B
>�B
?�B
>�B
?�B
?�B
?�B
@�B
A�B
B�B
C�B
B�B
A�B
A�B
C�B
B�B
A�B
B�B
@�B
=�B
B�B
B�B
A�B
A�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
G�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
I�B
G�B
E�B
G�B
J�B
K�B
K�B
K�B
M�B
N�B
M�B
M�B
M�B
MB
M�B
L�B
LB
M�B
NB
M�B
M�B
LB
MB
L�B
K�B
L�B
NB
NB
NB
OB
O�B
O�B
O�B
QB
Q�B
Q�B
RB
RB
Q�B
SB
R B
P.B
RB
RB
UB
TB
U2B
U2B
T,B
S@B
V9B
U2B
VB
W?B
XB
W?B
X+B
Y1B
Y1B
Y1B
Y1B
Z7B
XEB
\)B
\)B
\)B
[#B
YeB
ZQB
Z7B
Z7B
YeB
]/B
]IB
^OB
_VB
^OB
^OB
`\B
_VB
`\B
`\B
`\B
`BB
`\B
`BB
_VB
_VB
`\B
abB
abB
abB
abB
bhB
a|B
b�B
cTB
cnB
dtB
dZB
dZB
dtB
b�B
c�B
dtB
e`B
ezB
dtB
f�B
ffB
ffB
e�B
ezB
f�B
gmB
g�B
gmB
g�B
h�B
g�B
hsB
g�B
hsB
hsB
g�B
g�B
f�B
ezB
d�B
g�B
h�B
iyB
iyB
i�B
h�B
h�B
i�B
h�B
jB
jB
i�B
i�B
h�B
k�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
m�B
n�B
m�B
l�B
o�B
o�B
n�B
o�B
n�B
n�B
p�B
p�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804270033472018042700334720180427003347201806221240522018062212405220180622124052201804271406222018042714062220180427140622  JA  ARFMdecpA19c                                                                20180423093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180423003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180423003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180423003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180423003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180423003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180423003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180423003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180423003537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180423003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20180423005715                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180423153202  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180424000000  CF  PSAL_ADJUSTED_QCD�)�D�)�G�O�                JM  ARCAJMQC2.0                                                                 20180426153347  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180426153347  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050622  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034052  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                