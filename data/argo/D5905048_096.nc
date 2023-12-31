CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-03-08T00:35:37Z creation;2017-03-08T00:35:39Z conversion to V3.1;2019-12-19T08:13:18Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170308003537  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA  I2_0577_096                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��u��Q 1   @��v����@339�����d�rGE8�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C�)C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C6\C7��C9��C;��C=��C?��CA��CC��CF\CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�)C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"w
D"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�;�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D껅D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��TA��HA��;A��#A��/A��;A��/A��;A��#A��A���AǺ^Aǰ!AǙ�AǅA�t�A�p�A�r�A�hsA�bNA�C�A��`A�oA��#AőhA�ffA�XA�+A���A��
A�Aġ�Aė�A�p�A�O�A�?}A�/A��A��A�{A�oA�A��`A�AøRAå�AÍPA�dZA���A�M�A�ZA��FA��DA��`A�`BA��A�9XA���A�M�A�"�A�9XA�n�A�&�A��A��A�"�A��A���A���A�%A�z�A��\A�I�A�v�A�n�A��;A��yA� �A���A��TA��A��A��A�~�A���A�ffA��^A�  A�K�A�p�A�z�A���A�1'A�9XA���A��A��PA��A�K�A��wA��HA�1'A�^5A��wA��A�5?A�{Az�+Av��At$�Ap��An�uAm�AkC�Ai|�AhM�Ae�Ab-AaA_�PA^�A]�A[AY��AYC�AYAWt�AUK�ARM�AQ�
AQ%AN$�AMG�AK�AKC�AI�#AH1'AG�hAF��AF-AE�PAC�mA@�A?p�A>�A<�HA:�yA9�TA8jA6�RA5�
A3hsA2=qA1�7A0v�A-`BA,�/A+\)A)��A)�PA( �A&��A%�A%33A$��A#�7A#oA!�A��A?}A��A�yA�An�A�-AC�A(�AhsA�HA�A^5AbNA��Al�A�jA�
A�HA�-A;dA
JA	��A��A�!A�A`BA��Al�AE�A Q�@��!@�J@��u@�ƨ@��y@�x�@�Q�@�33@��#@�?}@�A�@�;d@���@��@@�@�@�F@�|�@�j@��@�\)@��T@�&�@�Z@�b@�|�@�R@�{@�/@�+@�E�@�V@�l�@⟾@�^5@�-@��@�@�C�@�V@���@�A�@ۥ�@�K�@���@ڗ�@�x�@ؼj@�K�@��@�A�@�\)@�K�@�o@��y@�o@҇+@ЋD@��`@��/@���@��@�%@���@�@�@��@�x�@�bN@�\)@��y@�-@�O�@�1'@ƸR@Ƈ+@ź^@�b@î@°!@�V@�@���@��@�33@�M�@���@���@�Q�@� �@�  @��m@���@�dZ@���@�n�@���@��@���@�I�@�l�@��\@�-@��@��@���@���@��@�K�@��@���@��+@�J@���@�@�&�@��@�9X@���@���@�t�@�l�@�@�{@���@�p�@�hs@�X@��@��/@��@�z�@��@�1@�1@��@���@�
=@�ff@���@���@�b@���@�K�@�ȴ@�~�@��@��-@��-@���@�x�@��@�r�@�1'@��w@�l�@�+@��R@�$�@��@���@��h@��7@�`B@�%@��`@��D@�Q�@� �@��@��
@�t�@��@�o@�@��@��H@�ȴ@���@���@���@�ff@�J@�-@�"�@�S�@�S�@�\)@�K�@�+@��@�o@��@�@��y@���@�V@���@���@���@�x�@��D@�I�@�ƨ@�\)@�o@���@�ff@�V@�{@���@�X@�/@���@��j@�j@�Z@�A�@�1@���@��
@�S�@��!@��+@�@���@��h@�%@��/@�Ĝ@��j@��9@���@���@��@�I�@��@��@�t�@�K�@�@���@��\@�ff@��@�J@��@��#@�hs@��/@���@��j@��j@��u@�1'@��w@�33@���@�ȴ@���@�~�@�=q@�n�@�$�@��h@�7L@���@��9@��@�bN@�Q�@�A�@�(�@�ƨ@���@�+@�@���@�V@�$�@���@���@���@�x�@�7L@��@��@���@��j@��u@�bN@�Z@�I�@�1'@��m@���@���@�l�@�\)@�S�@�
=@���@�n�@�^5@�V@�E�@�5?@�5?@�-@�$�@��@�J@���@��h@�?}@���@���@�9X@�b@�1@��@���@�|�@�dZ@�;d@�
=@��H@���@�^5@�5?@�J@���@��@�G�@���@��u@�Z@�Q�@�I�@�1'@��@�;@l�@~ȴ@~�+@~5?@}@}�-@}�T@}�-@}`B@|��@|�j@|�D@{��@{"�@y�7@y%@x��@xA�@x  @w�w@w�P@w;d@w�@w�@w�@w
=@v�y@vȴ@v�+@vv�@vff@u�T@u/@t�/@t�@st�@r=q@q��@q�^@p��@pr�@o|�@n��@n��@nv�@nff@m@mp�@m`B@m`B@m�@l�@lz�@lI�@l1@k�
@kt�@k@j��@j^5@i�#@i��@i&�@h��@h �@g��@gK�@f�y@f{@eO�@d��@d�@c�F@c��@c��@c�@co@a�^@`��@`�u@`�u@`�u@`�@_��@_l�@_\)@^��@^ȴ@^{@]@]p�@]O�@]/@\��@\�@[�m@[S�@[@Z��@Z~�@ZM�@Y��@Y�^@Yx�@X�@W��@W�P@W|�@W\)@W\)@W\)@W\)@W+@V�@Vv�@Vff@VE�@V@U�-@U�@U/@T�/@T�@TZ@T�@Sƨ@St�@SdZ@S33@R��@Rn�@RM�@R�@Q��@Q�@P�9@P��@P�u@P�u@Pr�@P �@O�;@O��@O�@O��@O|�@OK�@O+@N�@N�+@NE�@N$�@N@M�-@M?}@L�@L�/@L(�@K��@Kt�@K"�@J�H@J��@J�!@J~�@JJ@I��@I&�@HĜ@H�u@H�@G�@Gl�@G;d@F�+@E��@E`B@E?}@E�@D�@D�j@DZ@D(�@D1@Cƨ@C�@Ct�@CS�@CC�@C@B��@B�\@Bn�@B^5@BM�@B-@A�@A��@AX@A�@@�`@@�u@@A�@?�@?\)@?�@>�@>�R@>��@>��@>�+@>v�@=�@=?}@<�/@<�@<9X@;��@;33@:��@:��@:n�@:-@:J@9�^@9�@8A�@7�@7\)@7;d@7+@6��@6�+@6V@6$�@6@5��@5�@5?}@5V@4��@4�@4��@4z�@4(�@3�m@3�@333@2��@2M�@1��@1��@1X@1%@0��@0Q�@0A�@0 �@0  @/�;@/�@/�P@/|�@.�@.�+@.{@-��@-�-@-p�@-`B@-O�@-/@,��@,��@,��@,�j@+��@+dZ@+@*��@*��@*�\@*M�@*J@)��@)X@(��@(��@(1'@'�w@'�@'|�@';d@&��@&ȴ@&5?@&@%�T@%�T@%��@%p�@%?}@%/@%/@$��@$��@$z�@$j@$Z@#�
@#C�@#@"��@"�\@"n�@"^5@"M�@"-@!��@!��@!�7@!hs@!7L@ Ĝ@ �u@ �@ b@�@K�@+@��@ȴ@v�@5?@$�@�T@�h@p�@p�@p�@`B@�@�j@z�@z�@z�@9X@�
@��@�@t�@dZ@S�@�H@n�@�@�#@��@��@hs@X@7L@&�@��@��@�9@Q�@�;@��@l�@+@ȴ@v�@5?@@@��@p�@?}@V@�@��@�@�D@(�@�m@ƨ@��@dZ@dZ@C�@"�@�@��@�!@~�@=q@��@��@�^@hs@�@�`@�9@��@�u@r�@Q�@1'@b@�;@�P@;d@��@�y@ȴ@��@E�@5?@E�@{@@��@p�@�@��@�@z�@Z@��@�m@��@��@��@�m@ƨ@ƨ@��@�@t�@33@o@@
�!@
~�@
M�@
=q@
=q@	�@	��@	hs@	G�@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��TA��HA��;A��#A��/A��;A��/A��;A��#A��A���AǺ^Aǰ!AǙ�AǅA�t�A�p�A�r�A�hsA�bNA�C�A��`A�oA��#AőhA�ffA�XA�+A���A��
A�Aġ�Aė�A�p�A�O�A�?}A�/A��A��A�{A�oA�A��`A�AøRAå�AÍPA�dZA���A�M�A�ZA��FA��DA��`A�`BA��A�9XA���A�M�A�"�A�9XA�n�A�&�A��A��A�"�A��A���A���A�%A�z�A��\A�I�A�v�A�n�A��;A��yA� �A���A��TA��A��A��A�~�A���A�ffA��^A�  A�K�A�p�A�z�A���A�1'A�9XA���A��A��PA��A�K�A��wA��HA�1'A�^5A��wA��A�5?A�{Az�+Av��At$�Ap��An�uAm�AkC�Ai|�AhM�Ae�Ab-AaA_�PA^�A]�A[AY��AYC�AYAWt�AUK�ARM�AQ�
AQ%AN$�AMG�AK�AKC�AI�#AH1'AG�hAF��AF-AE�PAC�mA@�A?p�A>�A<�HA:�yA9�TA8jA6�RA5�
A3hsA2=qA1�7A0v�A-`BA,�/A+\)A)��A)�PA( �A&��A%�A%33A$��A#�7A#oA!�A��A?}A��A�yA�An�A�-AC�A(�AhsA�HA�A^5AbNA��Al�A�jA�
A�HA�-A;dA
JA	��A��A�!A�A`BA��Al�AE�A Q�@��!@�J@��u@�ƨ@��y@�x�@�Q�@�33@��#@�?}@�A�@�;d@���@��@@�@�@�F@�|�@�j@��@�\)@��T@�&�@�Z@�b@�|�@�R@�{@�/@�+@�E�@�V@�l�@⟾@�^5@�-@��@�@�C�@�V@���@�A�@ۥ�@�K�@���@ڗ�@�x�@ؼj@�K�@��@�A�@�\)@�K�@�o@��y@�o@҇+@ЋD@��`@��/@���@��@�%@���@�@�@��@�x�@�bN@�\)@��y@�-@�O�@�1'@ƸR@Ƈ+@ź^@�b@î@°!@�V@�@���@��@�33@�M�@���@���@�Q�@� �@�  @��m@���@�dZ@���@�n�@���@��@���@�I�@�l�@��\@�-@��@��@���@���@��@�K�@��@���@��+@�J@���@�@�&�@��@�9X@���@���@�t�@�l�@�@�{@���@�p�@�hs@�X@��@��/@��@�z�@��@�1@�1@��@���@�
=@�ff@���@���@�b@���@�K�@�ȴ@�~�@��@��-@��-@���@�x�@��@�r�@�1'@��w@�l�@�+@��R@�$�@��@���@��h@��7@�`B@�%@��`@��D@�Q�@� �@��@��
@�t�@��@�o@�@��@��H@�ȴ@���@���@���@�ff@�J@�-@�"�@�S�@�S�@�\)@�K�@�+@��@�o@��@�@��y@���@�V@���@���@���@�x�@��D@�I�@�ƨ@�\)@�o@���@�ff@�V@�{@���@�X@�/@���@��j@�j@�Z@�A�@�1@���@��
@�S�@��!@��+@�@���@��h@�%@��/@�Ĝ@��j@��9@���@���@��@�I�@��@��@�t�@�K�@�@���@��\@�ff@��@�J@��@��#@�hs@��/@���@��j@��j@��u@�1'@��w@�33@���@�ȴ@���@�~�@�=q@�n�@�$�@��h@�7L@���@��9@��@�bN@�Q�@�A�@�(�@�ƨ@���@�+@�@���@�V@�$�@���@���@���@�x�@�7L@��@��@���@��j@��u@�bN@�Z@�I�@�1'@��m@���@���@�l�@�\)@�S�@�
=@���@�n�@�^5@�V@�E�@�5?@�5?@�-@�$�@��@�J@���@��h@�?}@���@���@�9X@�b@�1@��@���@�|�@�dZ@�;d@�
=@��H@���@�^5@�5?@�J@���@��@�G�@���@��u@�Z@�Q�@�I�@�1'@��@�;@l�@~ȴ@~�+@~5?@}@}�-@}�T@}�-@}`B@|��@|�j@|�D@{��@{"�@y�7@y%@x��@xA�@x  @w�w@w�P@w;d@w�@w�@w�@w
=@v�y@vȴ@v�+@vv�@vff@u�T@u/@t�/@t�@st�@r=q@q��@q�^@p��@pr�@o|�@n��@n��@nv�@nff@m@mp�@m`B@m`B@m�@l�@lz�@lI�@l1@k�
@kt�@k@j��@j^5@i�#@i��@i&�@h��@h �@g��@gK�@f�y@f{@eO�@d��@d�@c�F@c��@c��@c�@co@a�^@`��@`�u@`�u@`�u@`�@_��@_l�@_\)@^��@^ȴ@^{@]@]p�@]O�@]/@\��@\�@[�m@[S�@[@Z��@Z~�@ZM�@Y��@Y�^@Yx�@X�@W��@W�P@W|�@W\)@W\)@W\)@W\)@W+@V�@Vv�@Vff@VE�@V@U�-@U�@U/@T�/@T�@TZ@T�@Sƨ@St�@SdZ@S33@R��@Rn�@RM�@R�@Q��@Q�@P�9@P��@P�u@P�u@Pr�@P �@O�;@O��@O�@O��@O|�@OK�@O+@N�@N�+@NE�@N$�@N@M�-@M?}@L�@L�/@L(�@K��@Kt�@K"�@J�H@J��@J�!@J~�@JJ@I��@I&�@HĜ@H�u@H�@G�@Gl�@G;d@F�+@E��@E`B@E?}@E�@D�@D�j@DZ@D(�@D1@Cƨ@C�@Ct�@CS�@CC�@C@B��@B�\@Bn�@B^5@BM�@B-@A�@A��@AX@A�@@�`@@�u@@A�@?�@?\)@?�@>�@>�R@>��@>��@>�+@>v�@=�@=?}@<�/@<�@<9X@;��@;33@:��@:��@:n�@:-@:J@9�^@9�@8A�@7�@7\)@7;d@7+@6��@6�+@6V@6$�@6@5��@5�@5?}@5V@4��@4�@4��@4z�@4(�@3�m@3�@333@2��@2M�@1��@1��@1X@1%@0��@0Q�@0A�@0 �@0  @/�;@/�@/�P@/|�@.�@.�+@.{@-��@-�-@-p�@-`B@-O�@-/@,��@,��@,��@,�j@+��@+dZ@+@*��@*��@*�\@*M�@*J@)��@)X@(��@(��@(1'@'�w@'�@'|�@';d@&��@&ȴ@&5?@&@%�T@%�T@%��@%p�@%?}@%/@%/@$��@$��@$z�@$j@$Z@#�
@#C�@#@"��@"�\@"n�@"^5@"M�@"-@!��@!��@!�7@!hs@!7L@ Ĝ@ �u@ �@ b@�@K�@+@��@ȴ@v�@5?@$�@�T@�h@p�@p�@p�@`B@�@�j@z�@z�@z�@9X@�
@��@�@t�@dZ@S�@�H@n�@�@�#@��@��@hs@X@7L@&�@��@��@�9@Q�@�;@��@l�@+@ȴ@v�@5?@@@��@p�@?}@V@�@��@�@�D@(�@�m@ƨ@��@dZ@dZ@C�@"�@�@��@�!@~�@=q@��@��@�^@hs@�@�`@�9@��@�u@r�@Q�@1'@b@�;@�P@;d@��@�y@ȴ@��@E�@5?@E�@{@@��@p�@�@��@�@z�@Z@��@�m@��@��@��@�m@ƨ@ƨ@��@�@t�@33@o@@
�!@
~�@
M�@
=q@
=q@	�@	��@	hs@	G�@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�DB�hB��B��B��B�B�!B�?B�?B�FB�XB�jBǮB�B�B��BDBuB�B�B�B{BoBuBuB�B�B�B �B!�B!�B!�B!�B"�B#�B#�B$�B'�B)�B-B8RBJ�Bn�B�7B�VB�hB�oB�{B�{B�oB�hB�\B�\B�=B� Bw�BjBm�Bq�Br�B|�B� B�B�+B�B~�B{�Bm�BcTB[#BVBR�BH�BD�B@�B2-B�B{BVB��B�mB��BȴB�3B��B�PBz�B]/BL�BB�B9XB0!B$�BVBB
�B
�B
��B
y�B
R�B
49B
!�B
JB	��B	��B	�mB	�
B	��B	�^B	��B	��B	�{B	�\B	�%B	~�B	u�B	n�B	k�B	e`B	]/B	I�B	D�B	B�B	33B	.B	)�B	#�B	�B	�B	uB	\B	JB	1B	B��B�B�B�B�ZB�BB�/B�
B��B��B��B��BɺBĜB��B�wB�9B�'B�B��B��B��B��B��B��B��B�hB�PB�DB�7B�%B�B�B� B~�B{�By�Bx�Bw�Bs�Bs�Bq�Br�B��B�hB�JB�+B�B�JB�VB�JB�hB��B�JB�B�Bu�Bo�Bo�Bn�Bl�BjBhsBgmBjBl�Bn�Bp�Bq�Br�Bv�B~�B~�B}�B~�B�B�uB��B�RB�wB�wB�qB�qB�}B��BBĜB��B�wB�wBBŢBƨBƨBǮBǮBǮBȴBɺBɺBɺB��B��B��B��B�B�
B�
B�B�B�B�B�#B�)B�;B�NB�`B�fB�fB�sB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	B	B	B	%B	%B	%B	%B	+B	1B	1B	DB	PB	VB	VB	VB	bB	hB	{B	�B	�B	�B	 �B	 �B	�B	�B	�B	�B	 �B	#�B	'�B	,B	.B	2-B	33B	33B	49B	6FB	;dB	<jB	<jB	<jB	=qB	>wB	@�B	@�B	C�B	F�B	H�B	I�B	I�B	I�B	K�B	L�B	L�B	M�B	N�B	O�B	O�B	O�B	Q�B	T�B	XB	[#B	]/B	aHB	bNB	cTB	e`B	ffB	jB	l�B	l�B	l�B	m�B	r�B	t�B	u�B	w�B	x�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�7B	�DB	�DB	�DB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�^B	�^B	�^B	�^B	�XB	�XB	�jB	�wB	��B	��B	B	B	ÖB	B	B	B	B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�NB	�TB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B
DB
DB
DB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
bB
hB
hB
oB
oB
oB
oB
oB
uB
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
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
-B
.B
.B
/B
/B
/B
/B
/B
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
6FB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
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
:^B
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
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
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
J�B
J�B
J�B
J�B
J�B
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
O�B
O�B
P�B
P�B
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
ZB
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
cTB
dZB
dZB
dZB
dZB
dZB
dZB
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
gmB
gmB
gmB
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
u�B
u�B
u�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�-B�DB��B��B��B�B�6B�;B�?B�?B�`B��B�BȴBٚB�9B��B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B!�B#B$B#�B%B(>B*�B.IB:BL�BpoB��B��B��B��B�9B��B�[B��B� B��B��B�B|6Bm�BnIBr�BtnB~�B��B�aB��B�9B��BBo�Bd�B\]BXBT{BI�BF?BEB5�B�B�BTB�B�B��B��B�zB��B�4B~�B_�BNpBD3B;B2�B(XB�B�B
��B
�B
�IB
�B
W?B
7�B
%�B
�B	��B	�fB	�B	�B	уB	�qB	��B	�vB	��B	�hB	��B	��B	v�B	o�B	m�B	h>B	`B	J�B	FYB	E�B	4�B	/�B	+6B	%�B	!�B	�B	�B	HB	�B	
�B	_B��B�B�'B��B��B�NB�VB��BּB҉B�(B��B��B��B�{B�4B�ZB��B��B�_B��B��B�XB�B�\B��B�oB��B�jB��B��B�B��B��B�B}B{�Bz�BzxBu�BuBrGBrGB��B��B�6B��B��B�B��B�B��B��B�B��B�aBv�BpoBp�BoiBm]Bk�BiyBhXBkkBm)BoiBqvBr�Bs�Bw�B�B�B.B~�B��B��B�zB�>B��B��B��B��B�B�'BÖB��B�uB�}B�}B�B��B��B�+BȚBȴBȀBɠB�#B�=B�	B�B�VBѷB��B�
B��B�+BؓB�+B�_B�KB�=B��B�\B�4B�zB�B�B��B�qB�B��B�B��B��B��B�XB��B��B��B	�B	oB	�B	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	pB	�B	�B	�B	B	�B	B	B	�B	!HB	!bB	 \B	 B	�B	�B	!-B	$tB	(�B	,qB	.cB	2aB	3hB	3�B	4TB	6zB	<B	<�B	<�B	<�B	=�B	>�B	@�B	AB	D3B	F�B	IB	I�B	I�B	I�B	K�B	MB	MB	NB	N�B	PB	PB	PHB	RoB	U�B	X�B	[�B	]�B	a�B	b�B	c�B	e�B	f�B	j�B	l�B	l�B	l�B	m�B	s3B	uB	vB	xB	y$B	zDB	|PB	~(B	B	�OB	�'B	�GB	�mB	�lB	�xB	�xB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�tB	�B	�AB	�-B	�MB	�ZB	�`B	�`B	�fB	�rB	�xB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�&B	�aB	�{B	�,B	�mB	�EB	�KB	�kB	�=B	�=B	�=B	�WB	�=B	�=B	�IB	�B	�B	�B	�tB	�B	�B	�B	��B	��B	��B	�B	�B	��B	�!B	�B	��B	��B	��B	��B	�B	�LB	�B	�B	��B	��B	�B	�B	��B	�6B	�PB	�<B	�BB	�(B	�(B	�B	�B	�B	�B	�HB
 OB
UB
AB
[B
AB
GB
-B
GB
3B
MB
MB
?B
?B
YB
_B
_B
EB
EB
EB
_B
fB
	lB

XB

XB

XB
^B
xB
�B
�B
jB
VB
pB
�B
VB
VB
�B
pB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
OB
�B
B
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
!B
 �B
 �B
!-B
# B
"�B
"B
"B
# B
# B
$B
$B
$�B
$�B
%B
&B
%�B
%�B
%�B
'B
'B
($B
($B
($B
(>B
)DB
)*B
)*B
*0B
*B
*0B
*0B
*KB
+6B
+6B
+6B
,WB
,WB
-CB
./B
.cB
/5B
/B
/OB
/iB
/�B
1vB
1AB
1'B
1AB
1AB
1vB
2aB
2GB
2aB
2GB
2aB
3MB
3MB
4TB
4TB
4nB
4nB
5ZB
5tB
5ZB
6zB
6`B
6`B
6`B
6zB
6zB
6�B
7�B
7LB
7fB
8RB
8RB
7LB
8RB
8lB
8lB
8lB
8lB
8lB
9rB
9rB
9�B
9�B
9rB
9rB
:xB
:xB
:xB
:�B
:xB
:�B
:�B
;B
;B
;B
;�B
;�B
<�B
<jB
<jB
<jB
<�B
=�B
=�B
=qB
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
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
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
MB
MB
L�B
L�B
L�B
MB
NB
NB
NB
N�B
N�B
OB
PB
PB
QB
Q B
Q B
Q B
Q B
QB
R:B
R:B
S&B
SB
TB
TB
TB
T,B
TB
U2B
U2B
UB
UB
U2B
VB
VB
VB
VB
V9B
VB
VB
W?B
W?B
W?B
X+B
XEB
X+B
XEB
Y1B
YKB
Y1B
YB
Y1B
Y1B
Z7B
Z7B
Z7B
ZQB
ZQB
[=B
[WB
[=B
[WB
\]B
\)B
\)B
\CB
]IB
]dB
]IB
]IB
]~B
^jB
_pB
_VB
_pB
_VB
_pB
_VB
_pB
`\B
`�B
`\B
`vB
abB
abB
b�B
b�B
bhB
bhB
b�B
cnB
cnB
cnB
c�B
cnB
cnB
dZB
cnB
dtB
d�B
d�B
dtB
d�B
d�B
e�B
ezB
f�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
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
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
y	B
x�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
z�B
{B
z�B
{B
z�B
|B
{�B
{�B
|B
|B
|B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201703120035102017031200351020170312003510201806221310182018062213101820180622131018201804050711162018040507111620180405071116  JA  ARFMdecpA19c                                                                20170308093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170308003537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170308003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170308003538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170308003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170308003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170308003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170308003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170308003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170308003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20170308010439                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170308153539  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20170308153539  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20170308153539  CV  LATITUDE        G�O�G�O�A��\                JM  ARCAJMQC2.0                                                                 20170311153510  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170311153510  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221116  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041018  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                