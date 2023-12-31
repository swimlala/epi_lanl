CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-26T00:35:25Z creation;2018-10-26T00:35:32Z conversion to V3.1;2019-12-19T07:29:34Z update;     
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
resolution        =���   axis      Z        ,  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     ,  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     ,  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ـ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181026003525  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              &A   JA  I2_0576_294                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؋�D�[�1   @؋�'�} @9�����d2�hr�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�C3Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�<�Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D���D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=p�@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B׸RB��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%�)C'��C)��C+��C-��C/��C2\C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&��D&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�
DU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa��Da�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�
D|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�A�Dс�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�A�D�~�D׾�D���D�>�D�~�Dؾ�D���D�;�D�~�Dپ�D���D�;�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�D໅D���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D��D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D��D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yA��TA��HA��;A��TA��HA��TA��;A��/A��/A��/A��/A��/A��/A��A��A��
A��
A��A��A̰!Aˡ�Aʇ+A�K�A�VA�bNAȅA��A��AŶFA�l�A�p�A��7A��#A���A��A�r�A���A�G�A�A�1A�n�A�ffA���A���A���A��#A�$�A�ƨA���A�`BA��^A�&�A�jA�;dA��A�M�A�1A��#A��A�A��TA�ffA���A�K�A��+A��TA�5?A�$�A��TA�-A�"�A�~�A��wA���A��!A�r�A�`BA��A�n�A�/A��TA��A��#A�|�A�ZA��A�5?A���A�VA��uA���A�5?A��DA��A�`BA��hA��A�{A~n�Az�!AvM�As�
ArA�Aq�wAp��Ap1An5?Am�Al��AjI�Agp�Afr�Ae�
AehsAc�-AbȴAb(�Aa�A`I�A_7LA^�A]��A[�PAXr�AW%AV��AV=qAU�AS�FAR{AQ�AQVAP��AP�\AO"�AN��ANv�AN �AMx�ALI�AKAJ{AIC�AI
=AHbAF��AFZAF�AE��AD��ACS�ACAB�`AB�AA�AA
=A?��A>�jA=��A=`BA=�A<I�A9��A8��A8�A6�yA6VA5�
A5`BA4��A4M�A3`BA21'A1�PA1?}A0��A0ffA/�A.��A-�A+ƨA*��A(�A'�#A&�A%ƨA$��A"��A"{A!�A!�wA!G�A ��A�
A��AffA^5A��A\)A��A�^AȴA7LAJAffA�AhsA`BAG�A"�A~�A�^AVA��A��Av�A^5A�#A�hAC�A�HA�jAz�A+A�A$�A��A�`AbNAJAoA��AoA~�Av�AQ�A�mA ��@���@���@�Q�@�ƨ@�ȴ@�`B@�A�@�@�5?@�x�@�b@�@��y@�!@��@�A�@��@��`@��@�+@�?}@��@�  @�  @�P@��@�-@��T@��#@�7@�P@�=q@��@�V@ߕ�@�=q@� �@�hs@ם�@�Q�@�+@љ�@�Ĝ@��m@ύP@�l�@�C�@Η�@��#@���@� �@�t�@�{@�Ĝ@���@��T@��@��
@��@�%@�
=@��@�A�@��@���@��j@�j@���@��+@��@��@��9@��u@��u@���@�bN@��
@�33@�@���@�^5@��@��@�^5@���@���@�|�@�@���@��h@���@���@�  @��\@�5?@��T@�7L@��9@��@�Q�@��@�Z@��@���@���@��@�K�@���@�v�@��#@�hs@�bN@�33@��T@�%@�Ĝ@�Q�@���@���@���@���@��h@�`B@�Q�@��m@��F@�l�@�l�@�\)@�n�@�-@�J@���@���@�`B@���@��/@�1'@�t�@���@�~�@�M�@�=q@�@���@���@�j@�Z@�Q�@��
@�33@��H@�v�@�{@��@��T@��-@�O�@���@���@�A�@���@�"�@��@��@��@��@���@���@���@���@���@���@���@���@��-@��-@���@���@�hs@�G�@�&�@���@��j@��@���@���@��@�bN@�9X@���@�|�@�S�@�C�@���@�~�@�V@�=q@�E�@�5?@�5?@�5?@�=q@�=q@�5?@�=q@�=q@�{@��@��^@���@��@�hs@�/@���@�Ĝ@���@���@���@�1'@�@�P@�P@�P@|�@|�@l�@|�@;d@�@+@~�y@~��@~ff@~V@~$�@~@}��@}�h@}p�@}`B@}p�@}�@|�/@|(�@{�
@{"�@z=q@yX@x��@x�9@x��@x��@x�`@x�`@xĜ@x�@xr�@x  @wK�@v�+@v@u��@u@uO�@t�@tZ@sdZ@s@r=q@p1'@n�@nE�@n@m@m�@m/@l�@lz�@lZ@k��@k�m@k�m@k�m@k�m@kC�@iX@hbN@g�@g�P@gK�@f�@f�R@f@d��@d�@dj@d�@c�
@c��@c�@cdZ@c33@c@b��@b��@b~�@b�@a��@a�^@a7L@`1'@_��@_��@_�P@_|�@_\)@_K�@^ȴ@]�@]�@\�j@\z�@\j@\j@\Z@\Z@\I�@\I�@\I�@\(�@\�@[�m@[S�@Z^5@Y�^@Y��@Yx�@YX@Y%@X �@WK�@V��@Vff@V5?@U@UV@T�@S��@S�
@S�F@S��@SS�@Q�#@P  @O+@N��@N{@M��@L��@Lj@L1@K��@KS�@K33@J�H@J~�@Jn�@J��@J��@J��@J�\@J~�@J^5@J=q@JJ@I��@I��@IG�@H��@H��@H�u@H�u@HA�@Hb@G�@G�@H  @Hb@Hb@G��@G�@Gl�@G\)@GK�@G;d@G;d@G;d@G;d@G+@G+@G
=@F��@FE�@E�-@E?}@D�@D��@D�D@D(�@C��@B�H@B��@B~�@B^5@B^5@B=q@A�@A�@A�#@A��@AX@A%@@�`@@�9@@b@?��@?�P@?K�@>ȴ@>��@>�+@>ff@>$�@=�@<�@<��@;�F@;o@:�H@:�H@:�H@:��@:��@:~�@9hs@7�@7|�@7K�@7+@6�R@6��@6��@6�+@6v�@6V@6{@6@5��@5�-@4I�@3C�@2��@2n�@2=q@2J@1�#@1��@1&�@0�9@0��@0��@0��@0��@0��@0�u@0bN@0bN@0r�@01'@0b@/�@/�@/�@/\)@.��@/
=@.ȴ@.�y@.�y@.�y@.��@.��@.�+@.{@-�@-p�@-p�@-`B@-O�@-?}@-/@-/@-�@-V@,�j@,�@,�@,�D@,I�@,(�@,(�@+�
@+�F@+��@+��@+��@+��@+��@+�@+S�@+33@+33@+"�@*��@*��@*��@*��@*^5@)��@)�7@(r�@(b@'��@'
=@&v�@&@%��@%�h@%�@%`B@%?}@%?}@%?}@%?}@%�@$�/@$��@$(�@#@"J@!��@!��@!��@!x�@!x�@!hs@!X@!G�@!%@ �9@ A�@\)@;d@��@�R@��@�+@�+@�+@v�@ff@E�@E�@5?@$�@�@�@��@@��@�h@p�@�@Z@9X@9X@9X@�@�@��@^5@�@��@�#@�^@x�@%@��@��@Ĝ@Ĝ@�9@�9@��@�u@�@bN@Q�@1'@ �@b@�;@�@;d@
=@
=@��@��@ȴ@v�@{@@��@�h@p�@O�@?}@?}@/@��@�@j@9X@�@1@��@�
@�
@�
@ƨ@��@�@C�@o@�@�H@�H@�H@�H@��@�!@��@J@�@%@�@��@��@��@�9@�9@Ĝ@Ĝ@�u@r�@�@�@�@r�@r�@�@r�@�@l�@v�@5?@�@�T@@��@�@`B@V@��@�D@�m@dZ@@
��@
M�@	�@	�^@	��@��@1'@��@��@�P@|�@l�@\)@K�@;d@+@�@�@
=@��@�y@�@�R@��@��@v�@v�@V@5?@@@{@{@{@@@@�@�@�T@�T@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yA��TA��HA��;A��TA��HA��TA��;A��/A��/A��/A��/A��/A��/A��A��A��
A��
A��A��A̰!Aˡ�Aʇ+A�K�A�VA�bNAȅA��A��AŶFA�l�A�p�A��7A��#A���A��A�r�A���A�G�A�A�1A�n�A�ffA���A���A���A��#A�$�A�ƨA���A�`BA��^A�&�A�jA�;dA��A�M�A�1A��#A��A�A��TA�ffA���A�K�A��+A��TA�5?A�$�A��TA�-A�"�A�~�A��wA���A��!A�r�A�`BA��A�n�A�/A��TA��A��#A�|�A�ZA��A�5?A���A�VA��uA���A�5?A��DA��A�`BA��hA��A�{A~n�Az�!AvM�As�
ArA�Aq�wAp��Ap1An5?Am�Al��AjI�Agp�Afr�Ae�
AehsAc�-AbȴAb(�Aa�A`I�A_7LA^�A]��A[�PAXr�AW%AV��AV=qAU�AS�FAR{AQ�AQVAP��AP�\AO"�AN��ANv�AN �AMx�ALI�AKAJ{AIC�AI
=AHbAF��AFZAF�AE��AD��ACS�ACAB�`AB�AA�AA
=A?��A>�jA=��A=`BA=�A<I�A9��A8��A8�A6�yA6VA5�
A5`BA4��A4M�A3`BA21'A1�PA1?}A0��A0ffA/�A.��A-�A+ƨA*��A(�A'�#A&�A%ƨA$��A"��A"{A!�A!�wA!G�A ��A�
A��AffA^5A��A\)A��A�^AȴA7LAJAffA�AhsA`BAG�A"�A~�A�^AVA��A��Av�A^5A�#A�hAC�A�HA�jAz�A+A�A$�A��A�`AbNAJAoA��AoA~�Av�AQ�A�mA ��@���@���@�Q�@�ƨ@�ȴ@�`B@�A�@�@�5?@�x�@�b@�@��y@�!@��@�A�@��@��`@��@�+@�?}@��@�  @�  @�P@��@�-@��T@��#@�7@�P@�=q@��@�V@ߕ�@�=q@� �@�hs@ם�@�Q�@�+@љ�@�Ĝ@��m@ύP@�l�@�C�@Η�@��#@���@� �@�t�@�{@�Ĝ@���@��T@��@��
@��@�%@�
=@��@�A�@��@���@��j@�j@���@��+@��@��@��9@��u@��u@���@�bN@��
@�33@�@���@�^5@��@��@�^5@���@���@�|�@�@���@��h@���@���@�  @��\@�5?@��T@�7L@��9@��@�Q�@��@�Z@��@���@���@��@�K�@���@�v�@��#@�hs@�bN@�33@��T@�%@�Ĝ@�Q�@���@���@���@���@��h@�`B@�Q�@��m@��F@�l�@�l�@�\)@�n�@�-@�J@���@���@�`B@���@��/@�1'@�t�@���@�~�@�M�@�=q@�@���@���@�j@�Z@�Q�@��
@�33@��H@�v�@�{@��@��T@��-@�O�@���@���@�A�@���@�"�@��@��@��@��@���@���@���@���@���@���@���@���@��-@��-@���@���@�hs@�G�@�&�@���@��j@��@���@���@��@�bN@�9X@���@�|�@�S�@�C�@���@�~�@�V@�=q@�E�@�5?@�5?@�5?@�=q@�=q@�5?@�=q@�=q@�{@��@��^@���@��@�hs@�/@���@�Ĝ@���@���@���@�1'@�@�P@�P@�P@|�@|�@l�@|�@;d@�@+@~�y@~��@~ff@~V@~$�@~@}��@}�h@}p�@}`B@}p�@}�@|�/@|(�@{�
@{"�@z=q@yX@x��@x�9@x��@x��@x�`@x�`@xĜ@x�@xr�@x  @wK�@v�+@v@u��@u@uO�@t�@tZ@sdZ@s@r=q@p1'@n�@nE�@n@m@m�@m/@l�@lz�@lZ@k��@k�m@k�m@k�m@k�m@kC�@iX@hbN@g�@g�P@gK�@f�@f�R@f@d��@d�@dj@d�@c�
@c��@c�@cdZ@c33@c@b��@b��@b~�@b�@a��@a�^@a7L@`1'@_��@_��@_�P@_|�@_\)@_K�@^ȴ@]�@]�@\�j@\z�@\j@\j@\Z@\Z@\I�@\I�@\I�@\(�@\�@[�m@[S�@Z^5@Y�^@Y��@Yx�@YX@Y%@X �@WK�@V��@Vff@V5?@U@UV@T�@S��@S�
@S�F@S��@SS�@Q�#@P  @O+@N��@N{@M��@L��@Lj@L1@K��@KS�@K33@J�H@J~�@Jn�@J��@J��@J��@J�\@J~�@J^5@J=q@JJ@I��@I��@IG�@H��@H��@H�u@H�u@HA�@Hb@G�@G�@H  @Hb@Hb@G��@G�@Gl�@G\)@GK�@G;d@G;d@G;d@G;d@G+@G+@G
=@F��@FE�@E�-@E?}@D�@D��@D�D@D(�@C��@B�H@B��@B~�@B^5@B^5@B=q@A�@A�@A�#@A��@AX@A%@@�`@@�9@@b@?��@?�P@?K�@>ȴ@>��@>�+@>ff@>$�@=�@<�@<��@;�F@;o@:�H@:�H@:�H@:��@:��@:~�@9hs@7�@7|�@7K�@7+@6�R@6��@6��@6�+@6v�@6V@6{@6@5��@5�-@4I�@3C�@2��@2n�@2=q@2J@1�#@1��@1&�@0�9@0��@0��@0��@0��@0��@0�u@0bN@0bN@0r�@01'@0b@/�@/�@/�@/\)@.��@/
=@.ȴ@.�y@.�y@.�y@.��@.��@.�+@.{@-�@-p�@-p�@-`B@-O�@-?}@-/@-/@-�@-V@,�j@,�@,�@,�D@,I�@,(�@,(�@+�
@+�F@+��@+��@+��@+��@+��@+�@+S�@+33@+33@+"�@*��@*��@*��@*��@*^5@)��@)�7@(r�@(b@'��@'
=@&v�@&@%��@%�h@%�@%`B@%?}@%?}@%?}@%?}@%�@$�/@$��@$(�@#@"J@!��@!��@!��@!x�@!x�@!hs@!X@!G�@!%@ �9@ A�@\)@;d@��@�R@��@�+@�+@�+@v�@ff@E�@E�@5?@$�@�@�@��@@��@�h@p�@�@Z@9X@9X@9X@�@�@��@^5@�@��@�#@�^@x�@%@��@��@Ĝ@Ĝ@�9@�9@��@�u@�@bN@Q�@1'@ �@b@�;@�@;d@
=@
=@��@��@ȴ@v�@{@@��@�h@p�@O�@?}@?}@/@��@�@j@9X@�@1@��@�
@�
@�
@ƨ@��@�@C�@o@�@�H@�H@�H@�H@��@�!@��@J@�@%@�@��@��@��@�9@�9@Ĝ@Ĝ@�u@r�@�@�@�@r�@r�@�@r�@�@l�@v�@5?@�@�T@@��@�@`B@V@��@�D@�m@dZ@@
��@
M�@	�@	�^@	��@��@1'@��@��@�P@|�@l�@\)@K�@;d@+@�@�@
=@��@�y@�@�R@��@��@v�@v�@V@5?@@@{@{@{@@@@�@�@�T@�T@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�yBDBVB
=BDB�BJBB��BDB.B49B49BF�BA�B<jBB�B/BB2-BA�BP�BR�B1'BXB\)B]/B`BBYBH�B9XB"�B
=B!�B+B!�BDB��B��B��B�B�B��B��B��BƨB��BĜB�9B��B��B�bB�Bs�Bz�Bz�Br�BdZB]/BR�B0!B�B"�BJB
��B
�B
�B
�B
�B
��B
��B
��B
�RB
�B
��B
�hB
z�B
W
B
5?B
"�B
�B
�B
�B
hB
DB	��B	�B	�B	�/B	�wB	��B	��B	ȴB	�XB	�FB	�FB	�B	��B	��B	��B	�\B	u�B	ffB	l�B	v�B	r�B	hsB	]/B	W
B	aHB	ZB	XB	XB	K�B	L�B	O�B	J�B	D�B	8RB	9XB	.B	+B	/B	&�B	�B	 �B	!�B	�B	oB	
=B	uB	uB	\B	%B��B��B��B�B��B�B�`B��B��B�
B��B��B��B��BɺBŢB�wB�LB�^B�wB�RB�FB�B��B��B��B�uB�=B�VB�JB�%B�B|�B�B�=B�+B�B~�Bv�By�Bl�BaHBm�Bn�BhsBaHB]/BR�BR�BL�BT�B`BB`BB^5BZBQ�BK�BJ�BE�BD�BM�BP�BK�BL�BK�BH�BH�BA�B33B%�B7LB=qB:^B<jB;dB6FB/B6FB6FB<jB9XB49B,B,B0!B(�B2-B0!B+B.B-B-B-B)�B+B1'B/B)�B�B �B$�B&�B#�B#�B'�B/B.B,B+B(�B,B,B'�B�B#�B%�B$�B�B�B�BhB�BuB#�B"�B)�B+B1'B33B33B1'B/B.B/B0!B.B-B$�B49B49B33B2-B-B-B/B;dB;dB7LB=qBD�BC�B=qBC�BJ�BK�BN�BP�BO�BM�BK�BM�BO�BM�BJ�BE�BE�BS�BS�BS�BVB^5B`BB[#B_;BdZBbNB`BBk�Bl�Bl�Br�Bx�Bx�Br�Be`B{�B� B�B�B�B� B� B� B�B~�B�B�%B�hB��B��B��B��B��B��B��B��B��B�!B�?B�FB�qB�qB�dBȴB��B��B��B��B��B�
B�B�/B�ZB�sB�B�B�B�B�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	+B	
=B	hB	\B	�B	#�B	$�B	$�B	$�B	$�B	&�B	&�B	'�B	(�B	(�B	+B	+B	.B	1'B	49B	5?B	8RB	9XB	9XB	9XB	:^B	:^B	;dB	;dB	A�B	A�B	@�B	D�B	F�B	H�B	J�B	K�B	N�B	O�B	P�B	Q�B	R�B	VB	XB	ZB	^5B	`BB	dZB	e`B	ffB	gmB	iyB	m�B	r�B	t�B	u�B	u�B	}�B	�+B	�=B	�=B	�DB	�JB	�JB	�PB	�JB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�FB	�RB	�^B	�^B	�^B	�dB	�XB	�^B	�RB	�FB	�^B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ɺB	ȴB	��B	��B	��B	ɺB	ǮB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�;B	�HB	�BB	�;B	�;B	�TB	�`B	�`B	�`B	�ZB	�ZB	�TB	�NB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B
B
1B
1B
	7B
1B
DB
JB
JB
PB
VB
VB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
%�B
&�B
(�B
(�B
'�B
+B
+B
)�B
)�B
+B
,B
,B
)�B
,B
.B
-B
-B
.B
.B
.B
-B
+B
.B
.B
,B
.B
1'B
2-B
1'B
1'B
0!B
/B
+B
+B
33B
49B
49B
33B
5?B
5?B
5?B
5?B
5?B
49B
5?B
49B
33B
1'B
33B
8RB
9XB
:^B
:^B
;dB
;dB
:^B
<jB
=qB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
=qB
=qB
>wB
>wB
=qB
<jB
<jB
?}B
>wB
@�B
?}B
>wB
>wB
>wB
>wB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
=qB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
@�B
?}B
?}B
>wB
@�B
B�B
@�B
A�B
C�B
D�B
F�B
F�B
G�B
G�B
H�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
F�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
K�B
O�B
P�B
O�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
R�B
R�B
R�B
S�B
R�B
R�B
R�B
R�B
T�B
VB
T�B
R�B
S�B
VB
W
B
W
B
XB
XB
XB
XB
XB
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
YB
[#B
\)B
\)B
\)B
[#B
ZB
[#B
]/B
\)B
\)B
]/B
]/B
^5B
^5B
]/B
]/B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
aHB
`BB
`BB
`BB
_;B
_;B
]/B
]/B
bNB
cTB
bNB
cTB
cTB
bNB
dZB
dZB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
cTB
cTB
bNB
`BB
`BB
`BB
e`B
e`B
gmB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
dZB
ffB
gmB
hsB
hsB
hsB
jB
jB
iyB
iyB
l�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�B�QB�BB�BBB(BtB��B(B0oB7�B8BH1BCB>]BDgB3�B�B88BG�BT�BW$B72BY�B]~B^B`�BZBJrB;B&�BpB# B+�B#BB �B iB�	B��B�BӏBөB�HB��B�BňB�B�,B�IB�:B�BvFB{dB{0Bs�Be�B^BT{B3�B"�B$&BB
��B
��B
��B
�B
�B
��B
��B
�B
��B
��B
��B
�@B
}�B
[�B
:*B
'�B
]B
CB
kB
�B
JB	��B	�%B	�B	�'B	��B	��B	̳B	ɆB	�JB	�fB	�2B	�;B	�RB	��B	��B	�4B	x�B	i�B	nB	wfB	s�B	i�B	_B	X�B	a�B	[=B	X�B	X�B	MPB	M�B	P.B	K^B	E�B	9�B	:DB	0B	,B	/�B	($B	!B	!�B	"NB	jB	�B	�B	�B	�B	�B	EB�HB�rB�B��B�+B�vB��BөBӏB�B�HBөBѝBΊBʦBƨB��B��B�0B��B�	B��B�!B��B��B�_B�2B�dB��B��B��B��BB��B�rB��B��B�BxBzxBn}Bc�BncBoiBi�Bb�B^�BUBT�BN�BVB`vB`vB^�BZ�BR�BMBK�BG+BE�BNVBQ4BL�BMPBLJBIRBIBB�B5ZB(�B88B>]B;dB=<B<6B7�B0�B7fB6�B<�B9�B5%B-�B-CB1B*B2�B0�B,"B.�B-�B-�B-�B+B+�B1[B/�B*�B!HB"4B%�B'�B%B$�B(�B/5B.IB,qB+�B)�B,WB,=B(�B 'B$�B&fB%�B�B�BB[BB�B$�B#�B*�B+�B1�B3�B3�B1�B/�B.�B/�B0�B/5B./B&LB4�B5B4B3B.�B.}B0�B<B<B8�B>(BEBDMB>�BDMBK)BLBN�BQBO�BN"BLJBNVBP.BN"BKxBF�BGBTaBT�BT�BV�B^�B`�B[�B_�Bd�Bb�Ba-Bk�Bl�Bm)BsBy>By>Bs�Bg�B|6B�4B�'B�AB�UB�iB��B��B��B�B�B�B��B��B�B�B�/B�]B�VB�*B�DB��B�oB�tB��B�qB��B�B��B��B�B�B�FB�MB�YB֡B��B��B��B�B��B��B��B�B�B��B��B�B�AB�+B�2B�B��B��B�$B�B�$B�*B�JB�dB	 iB	EB	
�B	�B	B	�B	#�B	$�B	$�B	$�B	$�B	'B	&�B	'�B	)B	)*B	+6B	+QB	./B	1[B	4�B	5�B	8lB	9�B	9rB	9�B	:�B	:�B	;�B	;�B	A�B	A�B	@�B	D�B	F�B	H�B	J�B	K�B	N�B	O�B	Q B	RB	S&B	VB	X+B	ZQB	^jB	`vB	d�B	ezB	f�B	g�B	i�B	m�B	r�B	t�B	u�B	vFB	~BB	�EB	�=B	�rB	�xB	�JB	�dB	�jB	�~B	�vB	�bB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�
B	�B	�B	�B	�"B	�)B	�)B	�/B	�OB	�iB	�|B	��B	�lB	�xB	��B	��B	��B	��B	��B	��B	�B	��B	��B	ªB	��B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�1B	�YB	�B	�B	�:B	�B	�:B	�B	�TB	�aB	�7B	�CB	�]B	�dB	�pB	�VB	�pB	�pB	�pB	�\B	�\B	�\B	�pB	�bB	�vB	ߊB	ߤB	�nB	�B	�zB	�zB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�%B	�B	��B	��B	�*B	�0B	�6B
B
 B
 B
 B
 OB	��B	��B
�B
fB
fB
	lB
�B
xB
~B
�B
jB
�B
�B
}B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
%�B
'B
(�B
)*B
(
B
+B
+B
*B
*0B
+B
,"B
,"B
*KB
,WB
./B
-CB
-CB
./B
./B
./B
-]B
+kB
./B
./B
,�B
.cB
1AB
2GB
1AB
1'B
0;B
/iB
+�B
+�B
33B
4nB
4TB
3hB
5?B
5?B
5?B
5ZB
5ZB
4TB
5tB
4TB
3�B
1�B
3�B
8�B
9rB
:xB
:xB
;�B
;B
:�B
<�B
=qB
>�B
>wB
>wB
>�B
=�B
=�B
>�B
>wB
=�B
=�B
>�B
>wB
=�B
<�B
<�B
?}B
>�B
@�B
?�B
>�B
>�B
>�B
>�B
<�B
=�B
>wB
?}B
?}B
?}B
?}B
>�B
>wB
>�B
>�B
=�B
?�B
?}B
?�B
>�B
?�B
?}B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
@�B
?�B
?�B
>�B
@�B
B�B
@�B
A�B
C�B
D�B
F�B
F�B
G�B
G�B
H�B
G�B
G�B
G�B
F�B
F�B
E�B
EB
F�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
L0B
O�B
Q B
PB
RB
Q�B
R�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
RB
R�B
S&B
SB
TB
S&B
SB
S&B
S&B
UB
VB
UB
S@B
TFB
V9B
W$B
W?B
X+B
X+B
XEB
X+B
X+B
Y1B
ZB
Z7B
ZB
ZB
Z7B
ZB
ZB
ZB
Z7B
ZB
ZQB
ZB
Z7B
Z7B
Z7B
YKB
[WB
\)B
\)B
\)B
[=B
Z7B
[=B
]/B
\]B
\]B
]IB
]IB
^OB
^5B
]IB
]dB
\CB
]IB
]IB
^jB
^OB
_;B
_VB
_;B
_;B
^OB
^OB
_pB
^OB
_VB
`BB
`\B
aHB
`BB
`BB
`\B
_VB
_pB
]�B
]~B
bhB
cTB
bhB
cTB
cnB
bhB
dtB
dtB
cTB
cnB
cTB
dZB
dZB
cTB
cTB
cnB
cTB
bhB
`�B
`vB
`�B
ezB
ezB
g�B
f�B
f�B
f�B
f�B
f�B
ezB
ezB
d�B
f�B
g�B
h�B
h�B
h�B
j�B
j�B
i�B
i�B
l�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810300036212018103000362120181030003621201810300200162018103002001620181030020016201810310025252018103100252520181031002525  JA  ARFMdecpA19c                                                                20181026093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181026003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181026003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181026003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181026003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181026003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181026003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181026003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181026003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181026003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20181026005727                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181026153721  CV  JULD            G�O�G�O�F�]�                JM  ARCAJMQC2.0                                                                 20181029153621  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181029153621  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181029170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181030152525  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                