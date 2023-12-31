CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-23T00:35:27Z creation;2017-08-23T00:35:30Z conversion to V3.1;2019-12-19T07:59:26Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20170823003527  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_152                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @� tzO� 1   @� u�b��@4j��f��d��PH�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C�)C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,w
D,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�
D8w
D8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�A�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AݸRAݺ^Aݴ9Aݡ�Aݡ�Aݡ�Aݝ�Aݝ�Aݛ�AݓuA�bNA�;dA�$�A��;A�O�A�n�A�oA�t�A��HA�ffAЕ�A�t�A�I�A�|�A��mA�jA�7LA�hsA�ȴA��mA�ZA�/A��yAç�A¥�A�\)A��RA�K�A�`BA�  A��hA��A�t�A�VA��
A��9A�Q�A��A��^A�^5A���A�XA�=qA��
A�E�A���A���A��
A��9A�O�A��A���A��uA�l�A�ƨA�=qA���A�/A���A��#A�~�A�I�A��A��/A��hA��
A��9A��^A�I�A�ƨA��A�oA��A�{A�A��A���A��jA�ffA���A�ĜA���A��^A��A�G�A�A���A�VA�bNA�^5A��#A�&�A��hA��A~bA{�;Az��Awl�Au\)AtjAsC�ArffAp�DAnz�Al��Ak��Aj��Ai�PAh��Ah�\AhjAg�Af�jAe�7Ad��AdM�Ac�^AbbA`�A^v�A[XAY|�AWS�AU7LAUVAT��ASK�AQ
=ANz�AMS�AL��AJ��AH��AG�7AF-AEt�AD�HADZAB�\A@{A>�!A> �A=��A=�^A=��A=\)A<��A;�mA9G�A6��A6$�A5�mA57LA3��A2�\A1��A1x�A1;dA0ĜA/�A,�jA*��A*9XA)7LA(  A&�A%%A"��A ��A��A��A�A�-A��A��A��AI�AhsA�RA$�A�AQ�A^5A�A
��A
E�A	��A�A�/AffA��AK�A�
A�A�^Ap�AE�A�!AXA�PAhsA��A�#A�A�DA z�@�;d@���@��u@�-@�A�@�^5@�$�@� �@��#@�u@�F@�R@홚@��@�M�@�C�@�^@�  @���@�V@�+@�r�@�l�@��@�=q@���@ٺ^@�p�@�X@�G�@�&�@�%@��/@�1'@֗�@�X@���@� �@�S�@���@ҸR@�X@��@��y@�`B@�Z@�5?@�1@�dZ@Ɵ�@�@�X@�&�@�(�@���@��@���@�9X@��@�C�@��@�~�@���@�O�@�/@��@��u@�1'@���@�@���@�E�@�{@�@��u@��w@�K�@�@���@���@��@��m@�S�@�=q@�Ĝ@�  @�ƨ@���@�K�@���@��!@��+@�V@�-@�@�@�7L@�Z@��@�\)@��@�@��R@�=q@�$�@�J@��#@���@�&�@�z�@��@��
@��P@�K�@�;d@�33@��@�
=@���@�=q@��@�@��@�@��^@��-@��-@��h@�p�@�/@���@�j@�bN@�Z@�(�@���@���@�l�@���@�E�@�5?@��@��@���@��h@�x�@�X@�?}@�&�@���@�Ĝ@��9@��D@�Q�@� �@���@��m@���@�l�@�S�@��@�~�@�E�@�5?@�@���@��@�%@�Ĝ@��9@��@���@���@��u@��@��m@��;@��@��@�;d@�
=@��@��!@�M�@��@�hs@��`@���@�bN@��@��;@��F@�o@��@�ȴ@�~�@�V@�@���@�X@��@�Ĝ@�z�@� �@���@��w@��P@�dZ@�33@��@�
=@��y@��H@��!@�v�@�{@���@��7@�7L@��@�%@�Ĝ@���@��u@�j@�Q�@�(�@��w@���@���@��@��@�l�@�C�@��@�
=@��@���@�E�@���@���@��@�hs@��@���@��@���@�r�@�A�@���@���@���@���@�dZ@�;d@�o@���@�^5@��T@��-@��7@�G�@�7L@�&�@�V@��@���@�j@�(�@��m@��@�;d@�
=@��H@���@�n�@�^5@�M�@�=q@�=q@�5?@�-@�J@��#@�@��7@�X@�7L@��/@��u@�r�@�I�@��@�ƨ@��P@��P@�|�@�;d@��R@�n�@�5?@���@��h@�O�@�/@�&�@���@��u@�A�@�@K�@~�+@~$�@}�-@}V@|�j@|z�@|�@{ƨ@{��@{�@{o@z�!@z~�@z=q@y�^@yX@x��@xr�@x �@w|�@w�@v�@v��@v�+@vE�@u�T@u��@u�@u�@t�@t��@tz�@t9X@t�@s�
@s�F@s�@s33@s@r��@rM�@q�@q�^@qx�@pĜ@p1'@p �@pb@pb@o��@o�P@o\)@o;d@n�R@m�@m�@m?}@mV@l�/@l�/@l�@l��@l�D@lZ@k�
@kS�@j�H@j~�@j-@jJ@i�#@i��@i��@i7L@i%@h��@h�@hbN@h  @f�+@e�@e��@e��@e�@e�@eV@d�@d�D@d(�@c�m@cdZ@b��@a�^@a�7@a�7@ahs@`��@`bN@_�@_l�@_�@^��@]��@]@]��@]�h@]p�@\��@\9X@[ƨ@[��@[33@[@Z�H@Z��@Z�\@Yx�@X�`@Xr�@X1'@X �@W�;@W�w@W�@WK�@Vȴ@V�+@V5?@U�T@U�h@T�@T�D@TZ@T1@S��@S�@St�@S"�@R�H@R~�@Q�^@Qhs@Q%@P��@P�9@Pr�@P  @O�P@O
=@N��@N$�@N$�@N@M�-@M�h@M�@Mp�@Mp�@M?}@L��@L��@L�j@L�j@L��@LI�@Kƨ@KdZ@K@J��@JM�@I��@IX@I7L@I%@HĜ@Hr�@H1'@G�@G�w@Gl�@Fȴ@FE�@F$�@F@Ep�@D��@D�j@DZ@C�m@C��@C�@CC�@B�@B��@B~�@B=q@A�@AG�@@�9@@�@@b@?��@?|�@?
=@>��@>v�@>E�@=�T@=��@=`B@=V@<��@<��@<��@<(�@<1@;ƨ@;��@;t�@;S�@;o@:��@:��@:-@:J@9�#@9��@9�@8�@8 �@7�@7�;@7�;@7�@7\)@6��@6�+@6v�@6v�@6E�@5�@5��@4�@4�j@4�D@4�D@4�D@4�D@4Z@3��@3�F@3t�@3C�@3"�@2�@2��@2=q@1��@1�^@0�`@0  @/�P@.��@.�+@.$�@-@-�@-/@-V@,�/@,�D@,9X@+�
@+t�@+o@+@*^5@)��@)G�@)�@(�`@(r�@(r�@(r�@(bN@( �@( �@(b@(  @'��@'K�@&��@&�@&��@&�+@&V@&{@&@%�T@%�@%p�@%`B@%O�@%/@%V@$�@$��@$j@$(�@#�
@#ƨ@#�F@#��@#��@#S�@"�H@"�!@"��@"��@"~�@"=q@!��@!�#@!�7@!G�@!7L@!�@!�@!�@!%@ ��@ �u@ b@�;@�@\)@+@�@ȴ@��@�+@�+@�+@V@E�@�@@�h@/@��@�@��@�D@Z@��@ƨ@ƨ@��@�@dZ@C�@@��@�!@��@=q@�@��@��@x�@G�@�@�`@�`@�`@��@�9@A�@b@�@��@l�@K�@+@��@ȴ@�R@ff@{@@�@��@�h@p�@?}@��@�@��@�j@��@z�@z�@I�@I�@9X@(�@1@�
@�F@dZ@"�@��@n�@^5@M�@=q@��@�@�^@��@hs@7L@&�@�@%@�`@�9@�@Q�@  @��@�P@|�@\)@+@�@�R@�R@��@ff@5?@5?@$�@$�@�@�-@�@`B@/@/@/@�@��@�@�j@z�@Z@9X@ƨ@�@t�@t�@S�@"�@
�@
��@
��@
�\@
^5@
M�@
=q@
J@	��@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AݸRAݺ^Aݴ9Aݡ�Aݡ�Aݡ�Aݝ�Aݝ�Aݛ�AݓuA�bNA�;dA�$�A��;A�O�A�n�A�oA�t�A��HA�ffAЕ�A�t�A�I�A�|�A��mA�jA�7LA�hsA�ȴA��mA�ZA�/A��yAç�A¥�A�\)A��RA�K�A�`BA�  A��hA��A�t�A�VA��
A��9A�Q�A��A��^A�^5A���A�XA�=qA��
A�E�A���A���A��
A��9A�O�A��A���A��uA�l�A�ƨA�=qA���A�/A���A��#A�~�A�I�A��A��/A��hA��
A��9A��^A�I�A�ƨA��A�oA��A�{A�A��A���A��jA�ffA���A�ĜA���A��^A��A�G�A�A���A�VA�bNA�^5A��#A�&�A��hA��A~bA{�;Az��Awl�Au\)AtjAsC�ArffAp�DAnz�Al��Ak��Aj��Ai�PAh��Ah�\AhjAg�Af�jAe�7Ad��AdM�Ac�^AbbA`�A^v�A[XAY|�AWS�AU7LAUVAT��ASK�AQ
=ANz�AMS�AL��AJ��AH��AG�7AF-AEt�AD�HADZAB�\A@{A>�!A> �A=��A=�^A=��A=\)A<��A;�mA9G�A6��A6$�A5�mA57LA3��A2�\A1��A1x�A1;dA0ĜA/�A,�jA*��A*9XA)7LA(  A&�A%%A"��A ��A��A��A�A�-A��A��A��AI�AhsA�RA$�A�AQ�A^5A�A
��A
E�A	��A�A�/AffA��AK�A�
A�A�^Ap�AE�A�!AXA�PAhsA��A�#A�A�DA z�@�;d@���@��u@�-@�A�@�^5@�$�@� �@��#@�u@�F@�R@홚@��@�M�@�C�@�^@�  @���@�V@�+@�r�@�l�@��@�=q@���@ٺ^@�p�@�X@�G�@�&�@�%@��/@�1'@֗�@�X@���@� �@�S�@���@ҸR@�X@��@��y@�`B@�Z@�5?@�1@�dZ@Ɵ�@�@�X@�&�@�(�@���@��@���@�9X@��@�C�@��@�~�@���@�O�@�/@��@��u@�1'@���@�@���@�E�@�{@�@��u@��w@�K�@�@���@���@��@��m@�S�@�=q@�Ĝ@�  @�ƨ@���@�K�@���@��!@��+@�V@�-@�@�@�7L@�Z@��@�\)@��@�@��R@�=q@�$�@�J@��#@���@�&�@�z�@��@��
@��P@�K�@�;d@�33@��@�
=@���@�=q@��@�@��@�@��^@��-@��-@��h@�p�@�/@���@�j@�bN@�Z@�(�@���@���@�l�@���@�E�@�5?@��@��@���@��h@�x�@�X@�?}@�&�@���@�Ĝ@��9@��D@�Q�@� �@���@��m@���@�l�@�S�@��@�~�@�E�@�5?@�@���@��@�%@�Ĝ@��9@��@���@���@��u@��@��m@��;@��@��@�;d@�
=@��@��!@�M�@��@�hs@��`@���@�bN@��@��;@��F@�o@��@�ȴ@�~�@�V@�@���@�X@��@�Ĝ@�z�@� �@���@��w@��P@�dZ@�33@��@�
=@��y@��H@��!@�v�@�{@���@��7@�7L@��@�%@�Ĝ@���@��u@�j@�Q�@�(�@��w@���@���@��@��@�l�@�C�@��@�
=@��@���@�E�@���@���@��@�hs@��@���@��@���@�r�@�A�@���@���@���@���@�dZ@�;d@�o@���@�^5@��T@��-@��7@�G�@�7L@�&�@�V@��@���@�j@�(�@��m@��@�;d@�
=@��H@���@�n�@�^5@�M�@�=q@�=q@�5?@�-@�J@��#@�@��7@�X@�7L@��/@��u@�r�@�I�@��@�ƨ@��P@��P@�|�@�;d@��R@�n�@�5?@���@��h@�O�@�/@�&�@���@��u@�A�@�@K�@~�+@~$�@}�-@}V@|�j@|z�@|�@{ƨ@{��@{�@{o@z�!@z~�@z=q@y�^@yX@x��@xr�@x �@w|�@w�@v�@v��@v�+@vE�@u�T@u��@u�@u�@t�@t��@tz�@t9X@t�@s�
@s�F@s�@s33@s@r��@rM�@q�@q�^@qx�@pĜ@p1'@p �@pb@pb@o��@o�P@o\)@o;d@n�R@m�@m�@m?}@mV@l�/@l�/@l�@l��@l�D@lZ@k�
@kS�@j�H@j~�@j-@jJ@i�#@i��@i��@i7L@i%@h��@h�@hbN@h  @f�+@e�@e��@e��@e�@e�@eV@d�@d�D@d(�@c�m@cdZ@b��@a�^@a�7@a�7@ahs@`��@`bN@_�@_l�@_�@^��@]��@]@]��@]�h@]p�@\��@\9X@[ƨ@[��@[33@[@Z�H@Z��@Z�\@Yx�@X�`@Xr�@X1'@X �@W�;@W�w@W�@WK�@Vȴ@V�+@V5?@U�T@U�h@T�@T�D@TZ@T1@S��@S�@St�@S"�@R�H@R~�@Q�^@Qhs@Q%@P��@P�9@Pr�@P  @O�P@O
=@N��@N$�@N$�@N@M�-@M�h@M�@Mp�@Mp�@M?}@L��@L��@L�j@L�j@L��@LI�@Kƨ@KdZ@K@J��@JM�@I��@IX@I7L@I%@HĜ@Hr�@H1'@G�@G�w@Gl�@Fȴ@FE�@F$�@F@Ep�@D��@D�j@DZ@C�m@C��@C�@CC�@B�@B��@B~�@B=q@A�@AG�@@�9@@�@@b@?��@?|�@?
=@>��@>v�@>E�@=�T@=��@=`B@=V@<��@<��@<��@<(�@<1@;ƨ@;��@;t�@;S�@;o@:��@:��@:-@:J@9�#@9��@9�@8�@8 �@7�@7�;@7�;@7�@7\)@6��@6�+@6v�@6v�@6E�@5�@5��@4�@4�j@4�D@4�D@4�D@4�D@4Z@3��@3�F@3t�@3C�@3"�@2�@2��@2=q@1��@1�^@0�`@0  @/�P@.��@.�+@.$�@-@-�@-/@-V@,�/@,�D@,9X@+�
@+t�@+o@+@*^5@)��@)G�@)�@(�`@(r�@(r�@(r�@(bN@( �@( �@(b@(  @'��@'K�@&��@&�@&��@&�+@&V@&{@&@%�T@%�@%p�@%`B@%O�@%/@%V@$�@$��@$j@$(�@#�
@#ƨ@#�F@#��@#��@#S�@"�H@"�!@"��@"��@"~�@"=q@!��@!�#@!�7@!G�@!7L@!�@!�@!�@!%@ ��@ �u@ b@�;@�@\)@+@�@ȴ@��@�+@�+@�+@V@E�@�@@�h@/@��@�@��@�D@Z@��@ƨ@ƨ@��@�@dZ@C�@@��@�!@��@=q@�@��@��@x�@G�@�@�`@�`@�`@��@�9@A�@b@�@��@l�@K�@+@��@ȴ@�R@ff@{@@�@��@�h@p�@?}@��@�@��@�j@��@z�@z�@I�@I�@9X@(�@1@�
@�F@dZ@"�@��@n�@^5@M�@=q@��@�@�^@��@hs@7L@&�@�@%@�`@�9@�@Q�@  @��@�P@|�@\)@+@�@�R@�R@��@ff@5?@5?@$�@$�@�@�-@�@`B@/@/@/@�@��@�@�j@z�@Z@9X@ƨ@�@t�@t�@S�@"�@
�@
��@
��@
�\@
^5@
M�@
=q@
J@	��@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVBVBT�BVBT�BT�BT�BT�BS�BP�BK�BJ�BF�B@�B8RB2-BoB\)BZB[#BdZBcTB^5BZBhsBk�B_;BM�BK�BR�B[#BdZBhsBu�B�B~�Bu�Bt�B�DB�bB�VB�VB��B��B��B�B�'B�B�B��B��B��B�=B��B��B��B��B��B�PB}�B|�BiyB\)BT�BQ�BJ�BB�B1'B0!B6FB6FB33B/B)�B"�B5?B5?B/B�B��B�B�NB��B��B��BŢB��B��B��B}�BT�B�B
�B
�mB
��B
�^B
��B
��B
�bB
k�B
k�B
`BB
VB
6FB
(�B
!�B
DB
  B
B	��B	��B	�`B	�#B	�B	��B	��B	ǮB	ǮB	ȴB	ŢB	��B	�FB	�B	�B	��B	��B	��B	�1B	x�B	`BB	[#B	L�B	D�B	L�B	S�B	J�B	=qB	5?B	33B	0!B	�B	1B	B��B��B��B�B�NB�#B�B�;B�5B�5B�#B�B��BŢB�LB�B�^B�XB�-B��B��B�B�B��B��B��B�1B�B�uB�\B�DB�%B}�Bt�Bk�BXBR�BR�BS�BM�BP�BO�BI�BN�BN�BK�BD�BG�B<jB@�BH�BH�BE�BD�BJ�BG�BD�BK�B]/BbNBbNBVBO�BcTBn�Bs�Bw�Bs�Bo�Bm�Bn�B^5BbNBhsBq�Bn�Bp�Bn�Bw�Br�Bs�Bz�By�B{�B{�Bq�By�Bp�Bp�Bp�Bn�Bq�Br�Be`Bp�Bv�B� B�B�B�%B�=B�DB�DB�PB�JB�7B�7B�JB�hB�hB�bB�hB�oB�\B�JB�\B�\B�\B�PB�DB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�?B�FB�3B�?B�FB�RB�jB�wB�}B�wB�qBƨB��B�B�;B�TB�yB�B�B�B�B��B	B	B	B	+B	PB	VB	bB	{B	�B	�B	�B	 �B	$�B	%�B	.B	33B	5?B	5?B	8RB	<jB	=qB	>wB	>wB	@�B	F�B	H�B	K�B	N�B	S�B	T�B	VB	YB	[#B	^5B	`BB	bNB	cTB	dZB	gmB	gmB	hsB	hsB	hsB	jB	n�B	p�B	v�B	w�B	v�B	y�B	z�B	}�B	}�B	�%B	�PB	�VB	�VB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�3B	�^B	�jB	�}B	��B	��B	��B	�}B	�wB	B	ĜB	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�;B	�NB	�NB	�ZB	�ZB	�`B	�fB	�`B	�fB	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
DB

=B
DB
DB
DB
JB
DB
DB
VB
VB
VB
PB
PB
bB
bB
\B
VB
bB
hB
hB
bB
oB
uB
uB
oB
bB
hB
hB
uB
hB
uB
uB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
$�B
$�B
&�B
&�B
'�B
&�B
%�B
#�B
%�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
)�B
(�B
)�B
,B
,B
,B
+B
+B
,B
-B
.B
-B
-B
0!B
0!B
0!B
/B
.B
0!B
1'B
2-B
1'B
2-B
2-B
2-B
1'B
0!B
2-B
33B
49B
5?B
49B
5?B
6FB
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
8RB
8RB
8RB
7LB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
=qB
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
>wB
@�B
A�B
A�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
C�B
C�B
B�B
C�B
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
E�B
F�B
G�B
G�B
H�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
O�B
O�B
O�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
S�B
S�B
S�B
R�B
R�B
R�B
Q�B
T�B
T�B
VB
VB
VB
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
T�B
T�B
W
B
W
B
XB
XB
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
[#B
ZB
ZB
\)B
]/B
]/B
]/B
_;B
_;B
_;B
^5B
_;B
_;B
_;B
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
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
dZB
ffB
ffB
ffB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
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
k�B
k�B
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
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
s�B
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
w�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
w�B
x�B
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
z�B
z�B
{�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVBVBUBVBT�BT�BT�BUBTBQNBL0BK)BG�BBAB;dB7�BQB^�B\�B\�Bd�Bd&B`BB]BiyBl�Ba|BQ�BO�BVB]�BgBkBw�B�B��By	BxRB�dB�:B�TB�B��B�RB��B��B��B� B�B�DB�FB�)B��B��B��B��B�VB��B�}B� BcBl=B^�BV�BS&BLBD�B4TB1�B7B6�B3�B/�B*�B$@B5�B5�B0�B�B-B�[B�,B�gBѷBѝB�_B��B��B��B��BZQBB
�?B
�B
�B
��B
�LB
��B
�uB
p�B
m)B
a�B
XB
:�B
+�B
#�B
�B
uB
?B	�dB	�2B	�B	ݘB	��B	ӏB	�B	��B	ȀB	�7B	�B	B	��B	��B	�B	��B	��B	��B	�XB	|B	c�B	]�B	OvB	F�B	MPB	T�B	MB	@iB	8B	4�B	1[B	 B	
�B	�B��B��B��B��B��B��BٴB��BޞB�jB�qB֡B�B�EB��B�B��B��B�hB�
B�6B��B��B��B��B��B��B��B�,B��B�B�KB�OBw�Bn�B\CBVmBUMBVBP.BRoBQhBK�BPBO�BL�BFYBH�B>�BBBIlBIRBF�BEmBKBH�BE�BL0B\�Bb�BcnBX�BP�Bb�BnBs�BxRBt�Bp�Bn�Bo�B`�Bc�Bi�Br�Bp;Bq�Bo�BxRBt9Bu?B{�Bz�B|�B|�BshBz�Br�Bq�Bq�BpBr�BtBgBq[BwLB��B�SB�mB�tB�XB�^B�xB��B��B�	B�XB�B��B��B� B��B��B�bB�PB�bB�}B�bB��B��B�$B�1B�_B�B�,B��B��B�vB�vB�hB��B�6B�]B��B��B�hB��B�zB��B��B��B��B��B��B��B��B�]B�EB�HB�_BߤB�&B�0B�/B�5B�}B�B�jB	GB	MB	mB	zB	�B	�B	�B	�B	�B	�B	B	!HB	%,B	&LB	.B	3�B	5�B	5�B	8lB	<�B	=�B	>�B	>�B	A B	GB	IB	K�B	OB	S�B	U2B	VB	YKB	[�B	^�B	`\B	bhB	cnB	d�B	gmB	g�B	h�B	h�B	h�B	j�B	n�B	qB	v�B	w�B	v�B	z*B	{JB	~(B	~�B	��B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�>B	�_B	�5B	�AB	�aB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	ªB	ĶB	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�6B	�.B	�4B	�:B	�&B	�,B	�[B	�?B	�+B	�?B	�_B	�YB	�_B	�KB	�qB	�qB	�]B	�~B	�pB	�hB	�B	�tB	�B	�zB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�"B	�<B	�6B	�<B	�BB
 B
'B
AB
-B
3B
MB
-B
GB
uB
GB
MB
MB
mB
_B
KB
fB
	lB

XB
^B
xB
^B
^B
^B
^B

rB
^B
xB
xB
~B
�B
xB
pB
�B
�B
�B
�B
bB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
�B
!B
 �B
!�B
"�B
#B
#�B
$B
$B
#�B
"�B
#B
#B
#B
#�B
$B
$�B
$�B
&B
$�B
%,B
'B
'B
(
B
'B
&B
$ZB
&B
'B
(
B
(
B
)*B
(>B
(
B
)*B
*KB
*0B
*0B
)_B
*eB
,=B
,"B
,"B
+6B
+6B
,WB
-]B
./B
-CB
-CB
0!B
0;B
0;B
/5B
.cB
0UB
1[B
2aB
1AB
2GB
2GB
2GB
1[B
0�B
2aB
3hB
4TB
5tB
4TB
5ZB
6zB
5tB
5tB
6zB
6`B
6`B
6�B
6zB
7fB
8�B
8�B
8lB
9rB
9rB
8lB
8lB
8�B
7�B
9rB
:xB
:xB
:xB
:�B
:�B
:�B
:�B
;�B
<�B
=qB
=�B
=�B
>wB
>wB
>wB
>wB
>�B
=�B
>�B
>�B
>wB
>�B
=�B
=�B
=�B
>�B
?�B
>�B
>�B
@�B
A�B
A�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
C�B
C�B
B�B
C�B
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
E�B
F�B
G�B
G�B
H�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
NB
O�B
PB
PB
OB
PB
QB
R B
RB
Q�B
RB
QB
Q4B
S�B
S�B
TB
S&B
S&B
S&B
R B
UB
UB
VB
VB
VB
U2B
U2B
UB
VB
VB
VB
VB
VB
VB
VB
V9B
UMB
UgB
W?B
W?B
X_B
X_B
Y1B
Y1B
ZQB
Z7B
Z7B
Z7B
Z7B
ZQB
ZkB
ZQB
[=B
Z�B
ZQB
\CB
]IB
]IB
]IB
_;B
_VB
_;B
^OB
_VB
_;B
_VB
^jB
^�B
_VB
`\B
`vB
`\B
`\B
`\B
abB
abB
`vB
abB
aHB
abB
abB
abB
a|B
abB
abB
abB
bhB
cTB
cTB
cTB
cnB
b�B
b�B
cnB
dZB
dtB
dtB
cnB
dtB
dtB
dtB
e�B
e`B
ezB
ffB
ffB
ezB
ezB
ezB
d�B
f�B
f�B
f�B
g�B
f�B
g�B
g�B
hsB
h�B
hsB
h�B
h�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
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
s�B
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
w�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
xB
w�B
w�B
x�B
x�B
xB
y	B
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
z�B
{B
{�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708270037102017082700371020170827003710201806221318052018062213180520180622131805201804050720232018040507202320180405072023  JA  ARFMdecpA19c                                                                20170823093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170823003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170823003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170823003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170823003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170823003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170823003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170823003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170823003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170823003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20170823005527                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170823153719  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170826153710  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170826153710  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222023  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041805  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                