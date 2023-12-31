CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-29T00:35:24Z creation;2018-04-29T00:35:30Z conversion to V3.1;2019-12-19T07:39:35Z update;     
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
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180429003524  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_235                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�^�����1   @�^�F)� @4�I�^5�dM:��S1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @7
=@}p�@��RA ��A\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�
D:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D��DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�ED�e1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���Aǰ!AǋDA�\)A��yA�C�A�1A�x�A�1'A��Aũ�A�S�A��Aě�A���AÓuA�?}A�ƨA�~�A�t�A�XA��A�A���A���A�Q�A�"�A�1A��A���A�l�A���A�=qA��wA�XA��;A���A���A�hsA��A�I�A�ƨA�O�A��/A�%A�oA���A�oA�t�A�A�~�A�(�A�=qA�K�A��#A�ȴA�n�A�A�A�A���A��HA�bNA� �A�7LA���A�$�A���A�bNA��A���A�n�A�7LA��TA�&�A�I�A�E�A��RA���A��A��!A�Q�A�ffA�~�A�dZA���A��A���A�
=A�1'A�&�A�l�A���A��TA��A��!A�|�A�"�A�ƨA�Q�A�C�A�  A}�PAz1'Aw�AvffAtQ�As
=Arv�AqC�An��AmC�AlA�Aj��Ah��Ae��Ab��Aa`BA_p�A]��A\bAZ�AY��AV1'ATVAS
=AQ��AN�HAM`BALZAKVAH�\AF�AE�TAC|�AA�A?�-A>1'A=oA<ZA;C�A:5?A9+A8Q�A6�\A4��A2ĜA1�A/��A/S�A.E�A,��A*�/A(��A'��A&M�A%��A$n�A#"�A"ȴA"�A v�A�A��A�AE�A�mAt�A�+A�wA��A1'A��AVA1A�A�A�wAĜAffAS�AĜAA�-A�7A%AG�A�;A�9A�A
VA�HA�^A�A��A1'A�^AƨAXAA�A  A�A z�A =qA @���@��9@�v�@�G�@�I�@���@��+@��`@�I�@�\)@�-@�/@�  @�\)@��@�5?@�`B@��m@���@�@�1'@�@���@�^@�h@�V@��m@�@�\@�?}@�  @�@ܬ@���@�v�@�E�@�{@ٙ�@�G�@�o@Ցh@�?}@��@�I�@�o@�7L@��/@υ@ΰ!@�^5@�%@�Q�@˥�@�K�@�?}@�1'@ǥ�@��y@�-@���@��@�r�@Å@��@�X@��`@�Q�@���@��@�G�@�A�@�  @��m@��
@���@��@�o@�G�@�Q�@� �@�ƨ@�\)@���@��T@�hs@���@� �@��@�S�@�+@���@�v�@��-@�G�@� �@��w@�t�@�S�@��+@�@��@��@��@� �@��;@��F@��@�+@��!@�=q@���@��7@��@��/@��j@�Z@��m@�ƨ@��@���@�5?@��@�hs@���@��@��u@�I�@��@���@�S�@�S�@�C�@�"�@���@��+@�E�@��T@���@�hs@�7L@�%@��9@�j@�1@��F@��P@�K�@��@�@��y@��@��!@��\@�ff@�E�@�{@�hs@�&�@�V@�%@��@���@�Ĝ@��@���@�z�@�9X@�ƨ@�dZ@�o@��@��y@�ȴ@�~�@�n�@�n�@�5?@�E�@�ȴ@�"�@��@�ƨ@��;@��
@� �@�z�@���@��9@�j@�I�@�1@��@�|�@��@���@�G�@��@�9X@��@�+@���@���@���@���@�@���@�J@��@��@��@���@��/@�Ĝ@�bN@�I�@���@�ƨ@�l�@�S�@��H@�^5@�@��7@��h@��-@�@��#@��@��@���@��^@���@�@���@�-@��h@�7L@�1@���@���@��@�Z@� �@�ƨ@�|�@��@���@��\@��+@�n�@�n�@�^5@�J@���@���@�`B@���@���@��@�Z@�A�@��@���@�l�@�l�@�K�@�+@�o@��@���@��!@�~�@�-@�-@�-@��@���@��h@�x�@�hs@�X@�7L@�V@���@��@��@��
@�S�@�+@�@��\@�V@�5?@�J@��^@��-@��^@���@���@��@�`B@�/@��`@��u@�z�@�Q�@���@��@�\)@�33@��@�@��H@��H@��@�ȴ@���@�n�@�$�@�@�@���@��@���@��@�1'@��@�@l�@�@~��@~ff@~5?@}��@}p�@|�/@|�D@|1@{ƨ@{ƨ@{ƨ@{�@{o@zn�@z�@yx�@x��@xr�@xA�@x �@w�;@w�w@w+@vV@u�@uV@t��@t�@t1@s��@s33@rn�@q�7@q�@p��@p�`@p��@pQ�@o�@o��@o|�@o;d@o�@n��@n�@n{@m�@m?}@l�j@lj@l9X@k��@kS�@k@j��@j~�@j�@i��@iX@i%@h�u@h �@g�w@f�@fV@fV@f5?@f$�@f@e@ep�@d�j@d�D@d�@b�!@a�#@a�#@a%@_�@_l�@_�@^��@^�y@^V@]�-@]?}@\z�@\�@[��@[�@[S�@["�@Zn�@Z�@Y��@Yx�@Yhs@Y�^@Y��@Y��@Y%@X�9@X�@W��@W;d@W
=@V��@V��@V��@VV@V5?@VV@U��@U?}@T��@T�/@T�j@T�@T��@U�@U�@U�@T��@T�@T�D@TI�@S��@S��@So@R�H@R�\@RM�@Q�@Q�7@Q&�@P��@Pr�@O|�@O\)@N�+@N5?@N@Mp�@MO�@M?}@M�@L�/@L�j@L�D@L1@Kƨ@K�@KS�@Ko@J�H@J=q@J-@I�@I��@I�^@I�^@I�^@I�7@H��@H��@Hr�@G�w@G;d@F�@Fff@F{@F@E`B@D�@D�/@D��@D��@D�j@DZ@C��@C��@B�H@Bn�@A�#@A�7@A�7@A7L@@Ĝ@@�u@@A�@@1'@@  @?\)@>�@>ȴ@>�R@>v�@>V@>E�@>5?@>@=�T@=�@=V@<�/@<��@<�@;�F@;�@;"�@:�\@:-@:J@9�#@9x�@97L@9%@8�`@8��@8��@8A�@7�@7|�@7
=@6�@6ff@5�T@5�h@5`B@5�@4�@4j@4I�@41@3�F@3o@2�!@2^5@2�@1�#@1�7@1X@1&�@0��@0Ĝ@0bN@0 �@0  @/�@/\)@/;d@/
=@.�@.��@.�+@.V@.{@-�-@-�@-`B@-O�@-/@-�@,��@,Z@,9X@,(�@,�@+�m@+��@+C�@*�!@*n�@*=q@)��@)��@)�7@)x�@)X@)X@)&�@(�9@(��@(�@(bN@(  @'\)@';d@'
=@&ȴ@&E�@%�@%�-@%��@%�h@%O�@%O�@%?}@%?}@%?}@%�@$�@$�@$I�@$�@$�@#��@#�m@#�
@#�@#"�@"�@"��@"�!@"��@"��@"�\@"=q@!�#@!��@!�7@!hs@!G�@!&�@ ��@ �9@ ��@ �u@ Q�@  �@   @��@�P@;d@��@��@��@��@ff@@��@�-@`B@�@V@��@��@�@z�@j@Z@1@�
@�F@C�@"�@�@��@��@M�@�#@�^@��@X@&�@%@Ĝ@�@�@bN@ �@�@�w@|�@\)@K�@
=@�y@�@��@�+@E�@{@�@��@�-@�@O�@V@��@��@��@�j@�@��@z�@I�@(�@��@ƨ@��@�@t�@@�H@��@��@�!@~�@M�@�@��@��@��@x�@hs@%@�9@A�@ �@  @�;@�@��@�P@l�@K�@�@��@�R@��@v�@5?@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���Aǰ!AǋDA�\)A��yA�C�A�1A�x�A�1'A��Aũ�A�S�A��Aě�A���AÓuA�?}A�ƨA�~�A�t�A�XA��A�A���A���A�Q�A�"�A�1A��A���A�l�A���A�=qA��wA�XA��;A���A���A�hsA��A�I�A�ƨA�O�A��/A�%A�oA���A�oA�t�A�A�~�A�(�A�=qA�K�A��#A�ȴA�n�A�A�A�A���A��HA�bNA� �A�7LA���A�$�A���A�bNA��A���A�n�A�7LA��TA�&�A�I�A�E�A��RA���A��A��!A�Q�A�ffA�~�A�dZA���A��A���A�
=A�1'A�&�A�l�A���A��TA��A��!A�|�A�"�A�ƨA�Q�A�C�A�  A}�PAz1'Aw�AvffAtQ�As
=Arv�AqC�An��AmC�AlA�Aj��Ah��Ae��Ab��Aa`BA_p�A]��A\bAZ�AY��AV1'ATVAS
=AQ��AN�HAM`BALZAKVAH�\AF�AE�TAC|�AA�A?�-A>1'A=oA<ZA;C�A:5?A9+A8Q�A6�\A4��A2ĜA1�A/��A/S�A.E�A,��A*�/A(��A'��A&M�A%��A$n�A#"�A"ȴA"�A v�A�A��A�AE�A�mAt�A�+A�wA��A1'A��AVA1A�A�A�wAĜAffAS�AĜAA�-A�7A%AG�A�;A�9A�A
VA�HA�^A�A��A1'A�^AƨAXAA�A  A�A z�A =qA @���@��9@�v�@�G�@�I�@���@��+@��`@�I�@�\)@�-@�/@�  @�\)@��@�5?@�`B@��m@���@�@�1'@�@���@�^@�h@�V@��m@�@�\@�?}@�  @�@ܬ@���@�v�@�E�@�{@ٙ�@�G�@�o@Ցh@�?}@��@�I�@�o@�7L@��/@υ@ΰ!@�^5@�%@�Q�@˥�@�K�@�?}@�1'@ǥ�@��y@�-@���@��@�r�@Å@��@�X@��`@�Q�@���@��@�G�@�A�@�  @��m@��
@���@��@�o@�G�@�Q�@� �@�ƨ@�\)@���@��T@�hs@���@� �@��@�S�@�+@���@�v�@��-@�G�@� �@��w@�t�@�S�@��+@�@��@��@��@� �@��;@��F@��@�+@��!@�=q@���@��7@��@��/@��j@�Z@��m@�ƨ@��@���@�5?@��@�hs@���@��@��u@�I�@��@���@�S�@�S�@�C�@�"�@���@��+@�E�@��T@���@�hs@�7L@�%@��9@�j@�1@��F@��P@�K�@��@�@��y@��@��!@��\@�ff@�E�@�{@�hs@�&�@�V@�%@��@���@�Ĝ@��@���@�z�@�9X@�ƨ@�dZ@�o@��@��y@�ȴ@�~�@�n�@�n�@�5?@�E�@�ȴ@�"�@��@�ƨ@��;@��
@� �@�z�@���@��9@�j@�I�@�1@��@�|�@��@���@�G�@��@�9X@��@�+@���@���@���@���@�@���@�J@��@��@��@���@��/@�Ĝ@�bN@�I�@���@�ƨ@�l�@�S�@��H@�^5@�@��7@��h@��-@�@��#@��@��@���@��^@���@�@���@�-@��h@�7L@�1@���@���@��@�Z@� �@�ƨ@�|�@��@���@��\@��+@�n�@�n�@�^5@�J@���@���@�`B@���@���@��@�Z@�A�@��@���@�l�@�l�@�K�@�+@�o@��@���@��!@�~�@�-@�-@�-@��@���@��h@�x�@�hs@�X@�7L@�V@���@��@��@��
@�S�@�+@�@��\@�V@�5?@�J@��^@��-@��^@���@���@��@�`B@�/@��`@��u@�z�@�Q�@���@��@�\)@�33@��@�@��H@��H@��@�ȴ@���@�n�@�$�@�@�@���@��@���@��@�1'@��@�@l�@�@~��@~ff@~5?@}��@}p�@|�/@|�D@|1@{ƨ@{ƨ@{ƨ@{�@{o@zn�@z�@yx�@x��@xr�@xA�@x �@w�;@w�w@w+@vV@u�@uV@t��@t�@t1@s��@s33@rn�@q�7@q�@p��@p�`@p��@pQ�@o�@o��@o|�@o;d@o�@n��@n�@n{@m�@m?}@l�j@lj@l9X@k��@kS�@k@j��@j~�@j�@i��@iX@i%@h�u@h �@g�w@f�@fV@fV@f5?@f$�@f@e@ep�@d�j@d�D@d�@b�!@a�#@a�#@a%@_�@_l�@_�@^��@^�y@^V@]�-@]?}@\z�@\�@[��@[�@[S�@["�@Zn�@Z�@Y��@Yx�@Yhs@Y�^@Y��@Y��@Y%@X�9@X�@W��@W;d@W
=@V��@V��@V��@VV@V5?@VV@U��@U?}@T��@T�/@T�j@T�@T��@U�@U�@U�@T��@T�@T�D@TI�@S��@S��@So@R�H@R�\@RM�@Q�@Q�7@Q&�@P��@Pr�@O|�@O\)@N�+@N5?@N@Mp�@MO�@M?}@M�@L�/@L�j@L�D@L1@Kƨ@K�@KS�@Ko@J�H@J=q@J-@I�@I��@I�^@I�^@I�^@I�7@H��@H��@Hr�@G�w@G;d@F�@Fff@F{@F@E`B@D�@D�/@D��@D��@D�j@DZ@C��@C��@B�H@Bn�@A�#@A�7@A�7@A7L@@Ĝ@@�u@@A�@@1'@@  @?\)@>�@>ȴ@>�R@>v�@>V@>E�@>5?@>@=�T@=�@=V@<�/@<��@<�@;�F@;�@;"�@:�\@:-@:J@9�#@9x�@97L@9%@8�`@8��@8��@8A�@7�@7|�@7
=@6�@6ff@5�T@5�h@5`B@5�@4�@4j@4I�@41@3�F@3o@2�!@2^5@2�@1�#@1�7@1X@1&�@0��@0Ĝ@0bN@0 �@0  @/�@/\)@/;d@/
=@.�@.��@.�+@.V@.{@-�-@-�@-`B@-O�@-/@-�@,��@,Z@,9X@,(�@,�@+�m@+��@+C�@*�!@*n�@*=q@)��@)��@)�7@)x�@)X@)X@)&�@(�9@(��@(�@(bN@(  @'\)@';d@'
=@&ȴ@&E�@%�@%�-@%��@%�h@%O�@%O�@%?}@%?}@%?}@%�@$�@$�@$I�@$�@$�@#��@#�m@#�
@#�@#"�@"�@"��@"�!@"��@"��@"�\@"=q@!�#@!��@!�7@!hs@!G�@!&�@ ��@ �9@ ��@ �u@ Q�@  �@   @��@�P@;d@��@��@��@��@ff@@��@�-@`B@�@V@��@��@�@z�@j@Z@1@�
@�F@C�@"�@�@��@��@M�@�#@�^@��@X@&�@%@Ĝ@�@�@bN@ �@�@�w@|�@\)@K�@
=@�y@�@��@�+@E�@{@�@��@�-@�@O�@V@��@��@��@�j@�@��@z�@I�@(�@��@ƨ@��@�@t�@@�H@��@��@�!@~�@M�@�@��@��@��@x�@hs@%@�9@A�@ �@  @�;@�@��@�P@l�@K�@�@��@�R@��@v�@5?@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�NB
�NB
�HB
�NB
�TB
�TB
�HB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�TB
�TB
�TB
��BVB�B$�B<jBS�Bz�B��B�!B��B�
B�B�B�B��B
=B{B �B'�B%�B�B(�B(�B!�B1'B@�BD�BA�B=qB1'B8RBG�BH�BN�BI�BM�BJ�BD�B>wBA�BJ�BM�BF�B;dB)�B$�B-B0!B33B0!B-B�B��BhB�B0!B/B&�B\B��B�B�B��B��B��B��B�B��B�B�B�fB�BǮB��B��B� B�1B`BBT�BS�BA�B%�B�B
�#B
��B
��B
��B
��B
��B
�{B
�DB
�B
�B
�B
�B
|�B
u�B
e`B
O�B
/B
JB
B	��B	��B	�B	�yB	�yB	�)B	ĜB	�wB	�qB	�!B	�{B	�B	o�B	n�B	e`B	VB	O�B	I�B	=qB	�B	�B	 �B	\B��B��B��B��B�ZB�;B�NB��BBɺBÖBĜBÖB�dB�LB�9B�B��B�{B�hB�PB�+B�{B�1Bw�Bv�Bo�Bx�Bv�B|�Bv�Bt�B�B{�Bm�Bl�Bw�By�Bv�Br�Bl�BdZBbNB`BBgmBiyB`BB^5BbNB]/BW
BT�B^5BXB\)BXB]/B\)BR�B?}B>wBD�BG�B9XB<jBF�BVBP�BK�BL�B:^BP�BXBT�BO�BK�BR�B[#BW
BN�BG�BH�BT�BZB\)B[#BXB`BB_;B[#B_;B]/BbNBdZB`BBaHB]/BdZBdZBbNBffBiyBs�Bt�Bs�Bn�Bv�Bp�Bl�Br�Bu�Bo�Bz�B�DB�\B�\B�JB�JB�B�DB��B��B��B�oB�hB��B��B��B��B��B��B��B��B��B�B�^B�dB�qB�}B��B�wB�qB�dBǮB��B��B��B��B��B��B�B�#B�)B�)B�/B�B�
B�#B�`B�ZB�mB�B�yB�B�B�B��B��B��B��B��B	B	1B	+B	bB	{B	�B	{B	�B	�B	�B	�B	#�B	&�B	)�B	)�B	+B	,B	2-B	5?B	<jB	=qB	@�B	C�B	B�B	E�B	J�B	J�B	I�B	M�B	R�B	R�B	W
B	]/B	]/B	]/B	^5B	_;B	bNB	ffB	ffB	ffB	ffB	ffB	jB	k�B	n�B	p�B	q�B	r�B	s�B	u�B	v�B	y�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�+B	�%B	�7B	�PB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�3B	�3B	�FB	�RB	�^B	�XB	�XB	�jB	�jB	�jB	�}B	�qB	�XB	�XB	�dB	�XB	�XB	�dB	�dB	�jB	��B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�
B	�
B	�/B	�HB	�`B	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�yB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
JB
PB
JB
JB
DB
PB
PB
PB
PB
JB
JB
JB
JB
PB
DB
PB
PB
PB
\B
bB
bB
bB
uB
{B
{B
{B
uB
uB
uB
uB
uB
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
"�B
"�B
!�B
!�B
"�B
#�B
$�B
$�B
%�B
$�B
$�B
#�B
"�B
#�B
%�B
%�B
%�B
%�B
$�B
%�B
&�B
&�B
&�B
'�B
&�B
$�B
$�B
&�B
&�B
'�B
'�B
'�B
&�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
-B
-B
-B
,B
+B
+B
)�B
+B
)�B
%�B
%�B
'�B
%�B
%�B
'�B
)�B
+B
+B
(�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
(�B
(�B
)�B
+B
+B
,B
.B
/B
/B
.B
/B
0!B
.B
0!B
0!B
1'B
0!B
1'B
1'B
2-B
2-B
1'B
33B
49B
6FB
6FB
6FB
8RB
9XB
:^B
:^B
9XB
9XB
:^B
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
:^B
9XB
<jB
:^B
<jB
=qB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
>wB
?}B
?}B
?}B
?}B
>wB
A�B
@�B
A�B
A�B
A�B
A�B
@�B
?}B
@�B
@�B
?}B
@�B
A�B
B�B
B�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
D�B
C�B
D�B
C�B
D�B
E�B
F�B
G�B
F�B
F�B
H�B
G�B
H�B
G�B
F�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
I�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
R�B
Q�B
S�B
S�B
T�B
T�B
T�B
VB
W
B
VB
VB
VB
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
ZB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
ZB
ZB
[#B
\)B
\)B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
_;B
_;B
^5B
^5B
`BB
`BB
`BB
_;B
`BB
aHB
bNB
bNB
aHB
bNB
cTB
bNB
bNB
bNB
aHB
bNB
bNB
cTB
dZB
cTB
cTB
cTB
bNB
bNB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
cTB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
e`B
ffB
ffB
ffB
e`B
ffB
ffB
gmB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
hsB
iyB
hsB
iyB
iyB
hsB
hsB
iyB
hsB
jB
jB
jB
jB
iyB
iyB
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
m�B
m�B
l�B
l�B
m�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
p�B
p�B
q�B
r�B
r�B
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
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�NB
�NB
�bB
�NB
�TB
�TB
�bB
�NB
�NB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�TB
�nB
��B
�0B�BCB%�B=qBTaBz^B�)B��B�AB��B�wB�|B�"B��B
�BgB!-B(
B&LB �B)_B)�B#nB2B@�BD�BB'B>wB3�B9�BH�BI�BO�BJ�BM�BK)BE�B?�BB�BK�BN�BG�B=VB,WB'�B.�B1vB49B1vB.}B �BAB�B!�B0�B/�B(>B�B��B��B�TB��B�B��B��B�B�tB�B�B�B�WB��B�nB�8B��B��BdBW�BU2BC�B(�B]B
�NB
ԯB
��B
��B
�"B
�,B
�mB
�6B
��B
��B
�B
��B
}�B
v�B
gB
R�B
2�B
�B
�B	��B	�]B	��B	�B	�B	��B	�_B	��B	��B	�B	��B	��B	r�B	p�B	g�B	X_B	Q�B	KxB	?}B	#�B	B	"�B	�B	 4B��B��B��B�B�|B��B��B�mB�xBňB�BĶB��B��B��B��B�4B�$B��B�\B��B�B��BzDBy>BrBzDBx�B}�Bx�Bv`B��B}"Bo�BnBx8Bz^BwLBshBm]Be�Bc�Ba�Bh
Bi�Ba�B_�BcTB^5BX�BVSB^�BYB\�BYB]�B\�BTBA�B@iBF?BIB;�B>]BHBV�BQ�BL�BM�B<�BQ�BX�BU�BP�BL�BS�B[qBW�BO�BI7BJ=BU�BZ�B\�B\BY1B`�B`B\B_�B^Bb�Bd�B`�Ba�B^jBe,Be,BcnBgRBjKBs�BuBt9Bo�BwLBqvBm�Bs�Bv�BqAB{�B�xB��B��B��B��B��B�JB��B�	B�?B�uB��B�;B��B�pB�LB��B�nB�sB�sB� B��B��B�B�B��B� B�B�BB�jB�1B�VB�\B�}BЗB�BѷB�EB�=B�CB�xB�~BںB�EB��B�zB��B��B�B�0B��B�)B�B�?B�0B�"B�BB�}B	�B	�B	B	�B	�B	�B	B	�B		B	B	 'B	$&B	'B	*KB	*0B	+kB	,qB	2�B	5�B	<�B	=�B	@�B	C�B	CB	FB	J�B	KB	JXB	N<B	S&B	SuB	W�B	]IB	]dB	]~B	^�B	_�B	b�B	ffB	f�B	f�B	f�B	f�B	j�B	k�B	n�B	p�B	q�B	r�B	tB	vB	w2B	zB	~(B	.B	�;B	�GB	�3B	�3B	�MB	�9B	�YB	�fB	�_B	��B	��B	��B	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�3B	�MB	�B	�8B	�^B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	ɠB	��B	͹B	��B	�B	�B	�0B	�B	�B	�(B	�B	�2B	�9B	�MB	�+B	�sB	�sB	�sB	�dB	�HB	�FB	�fB	�B	�B	�yB	�B	�B	�B	�qB	�B	�B	��B	��B	�$B	��B	�B	�B	�B	��B	�B	��B	�3B	��B	�B	��B	��B	��B	��B	�*B	�B	�B	�B	�PB
 4B
 4B
'B
GB
aB
GB
?B
1B
	RB
	RB
	RB
	RB

XB

XB

rB

rB
JB
PB
dB
dB
�B
�B
�B
jB
jB
�B
~B
�B
�B
�B
�B
jB
�B
�B
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
�B
�B
 B
 �B
 �B
 �B
"�B
#�B
#�B
#B
#B
"B
"B
#:B
$&B
%B
%B
&B
%,B
%B
$&B
# B
$B
%�B
&B
%�B
%�B
%B
%�B
'B
'B
'B
(
B
'B
%,B
%,B
'B
'8B
(
B
(
B
(
B
'B
)B
)B
)B
)*B
*B
*KB
*0B
*KB
*0B
*0B
*KB
+QB
-B
-)B
-CB
,=B
+B
+6B
*0B
+B
*0B
&fB
&2B
($B
&2B
&LB
($B
*B
+B
+6B
)*B
($B
($B
(>B
($B
)*B
*B
*0B
)B
)*B
*B
+B
+B
,B
-�B
/B
/5B
.IB
/OB
0;B
.}B
0UB
0;B
1'B
0UB
1'B
1[B
2GB
2-B
1[B
3MB
4TB
6FB
6`B
6`B
8RB
9>B
:^B
:^B
9rB
9rB
:xB
9rB
9rB
9�B
9�B
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
9�B
<�B
:�B
<�B
=�B
<�B
>�B
>�B
>�B
>�B
>�B
>�B
=�B
>�B
?�B
?�B
?�B
?�B
>�B
A�B
@�B
A�B
A�B
A�B
A�B
@�B
?�B
@�B
@�B
?�B
@�B
A�B
B�B
B�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
D�B
C�B
D�B
C�B
D�B
E�B
F�B
G�B
F�B
F�B
H�B
G�B
H�B
G�B
F�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
KB
I�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
NB
M�B
M�B
N�B
OB
N�B
O�B
OB
NB
N�B
OB
O(B
O�B
PB
PB
Q B
RB
RB
R B
SB
TB
T,B
S@B
RTB
TB
T,B
UB
U2B
U2B
VB
W?B
VB
VB
V9B
W$B
X+B
W$B
X+B
X+B
X+B
X+B
X+B
ZB
Y1B
Y1B
Y1B
Z7B
[=B
[#B
[WB
[=B
ZQB
Z7B
[=B
\)B
\CB
[=B
[=B
[WB
[WB
\]B
]IB
]IB
]IB
^5B
^OB
^OB
^OB
^OB
]~B
_;B
_pB
_VB
^�B
^jB
`vB
`\B
`\B
_pB
`\B
a|B
bNB
bNB
a|B
bNB
cnB
bhB
bNB
bhB
a|B
b�B
bhB
cnB
dZB
cnB
cnB
cnB
bhB
bhB
dtB
dtB
e`B
ezB
e`B
dtB
dtB
c�B
ezB
e�B
ezB
ezB
ezB
ezB
ffB
ffB
f�B
ezB
f�B
f�B
f�B
ezB
f�B
f�B
g�B
f�B
g�B
g�B
f�B
g�B
g�B
g�B
h�B
iyB
iyB
h�B
i�B
h�B
iyB
i�B
h�B
h�B
i�B
h�B
j�B
j�B
j�B
j�B
i�B
i�B
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
m�B
m�B
l�B
l�B
m�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
o�B
n�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
p�B
p�B
q�B
r�B
r�B
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
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805030037112018050300371120180503003711201806221329462018062213294620180622132946201806042132062018060421320620180604213206  JA  ARFMdecpA19c                                                                20180429093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180429003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180429003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180429003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180429003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180429003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180429003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180429003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180429003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180429003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180429005627                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180429153307  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180502153711  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180502153711  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123206  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042946  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                