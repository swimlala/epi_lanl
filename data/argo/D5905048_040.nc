CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-20T21:35:19Z creation;2016-09-20T21:35:22Z conversion to V3.1;2019-12-19T08:26:10Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160920213519  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA  I2_0577_040                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��r���1   @��s^З�@3����d�{���m1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @0��@}p�@��R@��RA ��A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�Q�A�Q�A�S�A�VA�VA�XA�XA�ZA�XA�XA�VA�S�A�S�A�Q�A�G�A�=qA�5?A�-A��A�JA���A��A��TA���A�RA��A�\A�t�A�^5A�M�A�5?A��A�
=A���A߲-A�jA�=qA�%A�v�A�"�A�/A�bNA�-A���A�oAҟ�A�Q�A���A���A��HA�l�A���Aʗ�A�C�AǓuA��A�{A�`BA��A��+A��RA��;A�?}A��hA��A�(�A��jA�$�A�bNA���A�G�A�n�A��HA��FA�$�A�ȴA���A���A�\)A��FA�|�A��A��A��A��mA�ƨA�/A���A�~�A��#A�ffA���A�M�A��`A�(�A��A�~�A��A�=qA��!A�A�A�;dA�9XA�1'A���A��A�O�A���A��\A�VA���A���A���A���A��+A���A�%A��7A��PA�A�A�oA��
A��A��A�ffA���A�ȴA�K�A�mA~A�AzffAxVAt��As�-AqdZAo��Ann�Am�Ak�-Ah�Af�+Aet�AcƨAb$�A`1'A]XA[��AYoAX�AW�mAV��AU��AU/AT�\AR�AP��AN{AL�\AK��AJ�AH��AG��AF�DAEK�AC�FAB$�AAA@�A@�A?G�A>��A=�A=+A<�RA;��A8�yA8{A6Q�A4Q�A3��A1��A0A/�A.��A.ZA-ƨA+x�A)hsA(�HA(~�A'�mA&��A$�A"bNA!;dA\)A�#A�DAx�A��A��A"�A��Ax�AƨA�A�AoA�AA
=A	�A��A�DAĜA
M�A	�#A	K�A	�A	��A��A��A�HAK�A�mA�FA�@��@�|�@��m@�1@�O�@�1'@��!@�hs@�S�@�ȴ@��@��@���@�%@�P@��@�(�@�K�@��@��@�-@�x�@�%@��@�j@�K�@��@�p�@�l�@ݡ�@�(�@ڸR@��@�j@ם�@��@�M�@���@��@�|�@�S�@Ұ!@ѡ�@��@�1@�"�@Η�@��#@���@�1@�ȴ@�V@�J@�O�@�(�@ǅ@��@�ȴ@�M�@��T@�`B@ċD@�9X@�o@�J@���@�/@���@��D@�r�@�(�@��
@�S�@��R@��@��^@�x�@���@��D@�(�@�  @��@��m@��
@��P@�@��R@�-@�p�@���@��u@�(�@��F@��y@��@��@��H@�n�@��@���@���@�`B@�%@��D@�9X@��@�|�@�o@���@���@�n�@�=q@��@���@�O�@���@���@���@�I�@��
@�t�@�S�@���@��@��T@���@�%@��D@�z�@�r�@�I�@� �@��@���@�\)@��y@���@�v�@�M�@�{@��@��-@�`B@�7L@��@��@��@�S�@�+@�
=@���@�v�@�n�@�E�@�@�@�X@���@�Ĝ@���@��D@�I�@���@�|�@�;d@���@��R@���@��+@�E�@�@��^@���@�(�@��P@�;d@���@���@��+@�E�@��T@�`B@���@���@��@�(�@���@���@���@�l�@�@��H@���@��!@���@�ff@�J@���@�J@���@��@�@��@��@��`@���@���@���@���@���@���@��9@��j@��/@���@��j@��@�r�@�(�@�  @���@��@��w@��P@�dZ@�+@�
=@��@��\@�M�@�5?@�{@��@��@��^@��@�X@�?}@���@�b@�ƨ@�l�@���@��@���@���@�n�@�-@�@��#@�@���@��@�?}@��`@�Ĝ@���@�bN@��@�l�@��R@�v�@�n�@�n�@�ff@�=q@���@�@�O�@�V@��/@��D@� �@�b@���@��;@��F@�33@�@��H@��!@���@��+@�^5@�=q@�5?@���@���@�O�@�?}@�&�@��@��/@�Z@��
@��w@���@��@�dZ@�C�@�33@���@���@�{@���@��@��@��T@���@��-@���@��7@�X@�&�@���@�bN@�A�@��;@��@���@�C�@���@�M�@��@���@��h@�hs@�`B@�`B@�?}@�V@��`@��j@���@�r�@� �@�@�P@+@~�R@~v�@~V@~5?@~@}?}@|�D@{ƨ@{C�@z��@z�!@z�\@z=q@z�@y�@y��@yhs@x��@xA�@w�@w|�@wl�@w�@v��@v{@u`B@tj@s�m@s�@r��@rn�@r�@q��@qX@p��@o��@o+@nv�@m�@m��@m/@m�@l�@l�D@k�m@kS�@j�@j�@j��@j�!@j-@i�7@iX@h�9@h �@g�@g�@g
=@fȴ@fff@fE�@e�@e��@e`B@d�/@dZ@d1@c�m@c��@c33@b��@b~�@b=q@a�^@a��@ahs@a7L@`��@`��@`A�@_�P@_K�@^ȴ@^��@^��@^�+@^ff@^{@]�h@\�@\�D@\Z@\I�@\1@[�
@[C�@Z�H@Z~�@Z�@ZJ@Y��@Y�^@Y�7@YX@Y&�@Y%@X�`@XĜ@X��@X1'@X  @W��@W|�@W�@VV@U�@U@U��@Up�@U`B@U?}@UV@T�D@S��@SdZ@So@R��@R�!@R^5@Q�#@Qx�@QX@Q�@P��@PQ�@P �@P  @O�;@O��@OK�@O
=@Nȴ@N��@NE�@N@M��@Mp�@L�@L�@L9X@L1@K�
@K��@KS�@K33@K33@K"�@J�H@J�\@J=q@I�#@Ix�@IX@I&�@H��@H�`@H�9@Hb@G�@G�w@G\)@G�@F�y@F��@Fff@FE�@F{@E@D�@D��@D�D@Dz�@Dj@Dj@D9X@C��@CS�@C@B�@B�H@B�!@B^5@B=q@A�#@Ax�@@�`@@bN@@  @?�;@?\)@?
=@>��@>�y@>��@>v�@>ff@>ff@>ff@>{@=@=?}@<�/@<�j@<z�@<�@;�
@;��@;dZ@;@:n�@:-@:J@9�@9��@9��@9��@9�7@9G�@9&�@8��@8��@8 �@7��@7�w@7K�@6ff@5O�@4��@4�/@4��@4��@41@3�@3C�@2��@2M�@2J@1�#@17L@0�`@0bN@01'@01'@0b@/�P@/+@.ȴ@.�+@.v�@.ff@.E�@.$�@-��@-�h@,�/@,��@,j@,9X@+�m@+��@+S�@+"�@+o@+o@*�@*�!@*�\@*n�@*M�@*=q@*J@)��@)��@)�#@)��@)G�@(Ĝ@(r�@(A�@(  @'�;@'�w@'�@'��@'��@'��@'�P@'|�@'�@'�@'
=@&�R@&V@&{@%�h@%/@%�@$��@$��@$��@$�D@$z�@$Z@$9X@#��@#S�@#"�@#@"��@"��@"~�@"^5@!��@!��@!�7@ �`@ �9@ ��@ �@ �@ r�@ r�@ Q�@ A�@  �@ b@�@��@|�@��@�+@V@�T@�@�@��@��@�/@�@�D@j@j@Z@��@�@�!@n�@~�@-@J@�#@�^@��@�7@7L@��@Ĝ@��@�u@bN@1'@ �@b@  @��@�P@l�@+@
=@
=@��@��@��@��@ff@$�@@��@�h@�h@�@`B@?}@�@�@�D@9X@1@�F@��@��@�@t�@t�@S�@33@o@�!@�\@~�@=q@�^@�7@7L@��@�u@bN@1'@�@�w@�@�@�@�@��@|�@K�@+@�y@��@V@{@��@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�VA�Q�A�Q�A�S�A�VA�VA�XA�XA�ZA�XA�XA�VA�S�A�S�A�Q�A�G�A�=qA�5?A�-A��A�JA���A��A��TA���A�RA��A�\A�t�A�^5A�M�A�5?A��A�
=A���A߲-A�jA�=qA�%A�v�A�"�A�/A�bNA�-A���A�oAҟ�A�Q�A���A���A��HA�l�A���Aʗ�A�C�AǓuA��A�{A�`BA��A��+A��RA��;A�?}A��hA��A�(�A��jA�$�A�bNA���A�G�A�n�A��HA��FA�$�A�ȴA���A���A�\)A��FA�|�A��A��A��A��mA�ƨA�/A���A�~�A��#A�ffA���A�M�A��`A�(�A��A�~�A��A�=qA��!A�A�A�;dA�9XA�1'A���A��A�O�A���A��\A�VA���A���A���A���A��+A���A�%A��7A��PA�A�A�oA��
A��A��A�ffA���A�ȴA�K�A�mA~A�AzffAxVAt��As�-AqdZAo��Ann�Am�Ak�-Ah�Af�+Aet�AcƨAb$�A`1'A]XA[��AYoAX�AW�mAV��AU��AU/AT�\AR�AP��AN{AL�\AK��AJ�AH��AG��AF�DAEK�AC�FAB$�AAA@�A@�A?G�A>��A=�A=+A<�RA;��A8�yA8{A6Q�A4Q�A3��A1��A0A/�A.��A.ZA-ƨA+x�A)hsA(�HA(~�A'�mA&��A$�A"bNA!;dA\)A�#A�DAx�A��A��A"�A��Ax�AƨA�A�AoA�AA
=A	�A��A�DAĜA
M�A	�#A	K�A	�A	��A��A��A�HAK�A�mA�FA�@��@�|�@��m@�1@�O�@�1'@��!@�hs@�S�@�ȴ@��@��@���@�%@�P@��@�(�@�K�@��@��@�-@�x�@�%@��@�j@�K�@��@�p�@�l�@ݡ�@�(�@ڸR@��@�j@ם�@��@�M�@���@��@�|�@�S�@Ұ!@ѡ�@��@�1@�"�@Η�@��#@���@�1@�ȴ@�V@�J@�O�@�(�@ǅ@��@�ȴ@�M�@��T@�`B@ċD@�9X@�o@�J@���@�/@���@��D@�r�@�(�@��
@�S�@��R@��@��^@�x�@���@��D@�(�@�  @��@��m@��
@��P@�@��R@�-@�p�@���@��u@�(�@��F@��y@��@��@��H@�n�@��@���@���@�`B@�%@��D@�9X@��@�|�@�o@���@���@�n�@�=q@��@���@�O�@���@���@���@�I�@��
@�t�@�S�@���@��@��T@���@�%@��D@�z�@�r�@�I�@� �@��@���@�\)@��y@���@�v�@�M�@�{@��@��-@�`B@�7L@��@��@��@�S�@�+@�
=@���@�v�@�n�@�E�@�@�@�X@���@�Ĝ@���@��D@�I�@���@�|�@�;d@���@��R@���@��+@�E�@�@��^@���@�(�@��P@�;d@���@���@��+@�E�@��T@�`B@���@���@��@�(�@���@���@���@�l�@�@��H@���@��!@���@�ff@�J@���@�J@���@��@�@��@��@��`@���@���@���@���@���@���@��9@��j@��/@���@��j@��@�r�@�(�@�  @���@��@��w@��P@�dZ@�+@�
=@��@��\@�M�@�5?@�{@��@��@��^@��@�X@�?}@���@�b@�ƨ@�l�@���@��@���@���@�n�@�-@�@��#@�@���@��@�?}@��`@�Ĝ@���@�bN@��@�l�@��R@�v�@�n�@�n�@�ff@�=q@���@�@�O�@�V@��/@��D@� �@�b@���@��;@��F@�33@�@��H@��!@���@��+@�^5@�=q@�5?@���@���@�O�@�?}@�&�@��@��/@�Z@��
@��w@���@��@�dZ@�C�@�33@���@���@�{@���@��@��@��T@���@��-@���@��7@�X@�&�@���@�bN@�A�@��;@��@���@�C�@���@�M�@��@���@��h@�hs@�`B@�`B@�?}@�V@��`@��j@���@�r�@� �@�@�P@+@~�R@~v�@~V@~5?@~@}?}@|�D@{ƨ@{C�@z��@z�!@z�\@z=q@z�@y�@y��@yhs@x��@xA�@w�@w|�@wl�@w�@v��@v{@u`B@tj@s�m@s�@r��@rn�@r�@q��@qX@p��@o��@o+@nv�@m�@m��@m/@m�@l�@l�D@k�m@kS�@j�@j�@j��@j�!@j-@i�7@iX@h�9@h �@g�@g�@g
=@fȴ@fff@fE�@e�@e��@e`B@d�/@dZ@d1@c�m@c��@c33@b��@b~�@b=q@a�^@a��@ahs@a7L@`��@`��@`A�@_�P@_K�@^ȴ@^��@^��@^�+@^ff@^{@]�h@\�@\�D@\Z@\I�@\1@[�
@[C�@Z�H@Z~�@Z�@ZJ@Y��@Y�^@Y�7@YX@Y&�@Y%@X�`@XĜ@X��@X1'@X  @W��@W|�@W�@VV@U�@U@U��@Up�@U`B@U?}@UV@T�D@S��@SdZ@So@R��@R�!@R^5@Q�#@Qx�@QX@Q�@P��@PQ�@P �@P  @O�;@O��@OK�@O
=@Nȴ@N��@NE�@N@M��@Mp�@L�@L�@L9X@L1@K�
@K��@KS�@K33@K33@K"�@J�H@J�\@J=q@I�#@Ix�@IX@I&�@H��@H�`@H�9@Hb@G�@G�w@G\)@G�@F�y@F��@Fff@FE�@F{@E@D�@D��@D�D@Dz�@Dj@Dj@D9X@C��@CS�@C@B�@B�H@B�!@B^5@B=q@A�#@Ax�@@�`@@bN@@  @?�;@?\)@?
=@>��@>�y@>��@>v�@>ff@>ff@>ff@>{@=@=?}@<�/@<�j@<z�@<�@;�
@;��@;dZ@;@:n�@:-@:J@9�@9��@9��@9��@9�7@9G�@9&�@8��@8��@8 �@7��@7�w@7K�@6ff@5O�@4��@4�/@4��@4��@41@3�@3C�@2��@2M�@2J@1�#@17L@0�`@0bN@01'@01'@0b@/�P@/+@.ȴ@.�+@.v�@.ff@.E�@.$�@-��@-�h@,�/@,��@,j@,9X@+�m@+��@+S�@+"�@+o@+o@*�@*�!@*�\@*n�@*M�@*=q@*J@)��@)��@)�#@)��@)G�@(Ĝ@(r�@(A�@(  @'�;@'�w@'�@'��@'��@'��@'�P@'|�@'�@'�@'
=@&�R@&V@&{@%�h@%/@%�@$��@$��@$��@$�D@$z�@$Z@$9X@#��@#S�@#"�@#@"��@"��@"~�@"^5@!��@!��@!�7@ �`@ �9@ ��@ �@ �@ r�@ r�@ Q�@ A�@  �@ b@�@��@|�@��@�+@V@�T@�@�@��@��@�/@�@�D@j@j@Z@��@�@�!@n�@~�@-@J@�#@�^@��@�7@7L@��@Ĝ@��@�u@bN@1'@ �@b@  @��@�P@l�@+@
=@
=@��@��@��@��@ff@$�@@��@�h@�h@�@`B@?}@�@�@�D@9X@1@�F@��@��@�@t�@t�@S�@33@o@�!@�\@~�@=q@�^@�7@7L@��@�u@bN@1'@�@�w@�@�@�@�@��@|�@K�@+@�y@��@V@{@��@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B]/B]/B^5B_;B^5B^5B_;B_;B`BBe`Be`Bk�Bt�Bu�Bw�B� B�%B�7B�VB��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�RB�RB�wB�
B�/B�mB�sB�ZB�sB�B��BɺB��B��B��B�B)�B7LB1'B2-B@�BI�B7LB�B{B(�BC�BN�BaHB� B�DB��B�BĜB��B��B��B��B��B��B��B��B�?B�B�!B�JBs�B�uB�9B�wBÖB�wB�XB�9B�B��B��B��B��B�DB�B}�Bx�Bn�BbNBZBT�BI�B7LBbB\B\B\BPBB��B�B��B�-B��B�1Bz�Bx�Bv�Bn�Be`BQ�B@�B,B�BDB
��B
�sB
�)B
��B
�jB
�B
�uB
�=B
� B
hsB
VB
@�B
1'B
$�B
�B
bB
+B	��B	�B	�)B	��B	��B	�wB	�9B	��B	��B	�DB	�7B	�B	|�B	t�B	q�B	n�B	dZB	XB	J�B	B�B	<jB	9XB	.B	(�B	!�B	�B	{B	JB	%B	B	B��B��B��B��B��B�B�yB�NB�5B��B��BɺBÖB�}B�qB�dB�dB�FB�B�B�B��B��B��B��B��B��B�hB�\B�=B�7B�B�B�B~�B}�B�B�%B�1B�B~�B|�B{�Bx�Bx�B}�B��B��B��B�B�B�B��B��B��B~�B� B�%Bu�Bw�B�B{�Bx�B��B�3B�!B�B�B�'B�!B�B�B�!B�'B�B�B�'B�-B�-B�-B�'B�B��B��B��B�'B�9B�9B�?B�?B�FB�FB�LB�XB�XB�wB�}B��B��BÖBƨBɺB��B��B��B��B�)B�BB�`B�sB�B�B��B��B��B	  B	B	+B		7B		7B	1B	
=B	DB	JB	PB	PB	VB	VB	\B	\B	oB	�B	�B	�B	�B	"�B	%�B	+B	,B	,B	,B	-B	.B	2-B	49B	7LB	<jB	<jB	<jB	?}B	C�B	F�B	J�B	M�B	N�B	VB	ZB	\)B	]/B	`BB	aHB	bNB	bNB	bNB	dZB	e`B	e`B	iyB	jB	k�B	l�B	m�B	q�B	r�B	s�B	u�B	w�B	|�B	~�B	� B	�B	�B	�B	�B	�+B	�=B	�=B	�DB	�JB	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�^B	�^B	�jB	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�5B	�;B	�HB	�HB	�BB	�NB	�TB	�ZB	�fB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
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
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
oB
hB
hB
oB
oB
oB
oB
oB
oB
uB
oB
oB
oB
oB
oB
hB
hB
bB
bB
bB
bB
bB
bB
\B
bB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
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
,B
,B
,B
,B
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
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
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
8RB
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
;dB
;dB
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
?}B
?}B
@�B
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
B�B
B�B
C�B
C�B
C�B
C�B
C�B
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
E�B
E�B
E�B
E�B
E�B
E�B
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
K�B
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
O�B
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
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
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
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
ffB
ffB
ffB
ffB
ffB
ffB
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
jB
jB
iyB
jB
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
p�B
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
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B]/B]/B^5B_;B^5B^5B_;B_;B`BBe`BezBk�Bt�Bu�Bw�B�B�%B�RB��B��B��B��B��B�B�B�*B�0B�=B�IB�UB�aB��B��B�XB�OB׍BݲB�
B�B�B�B�B��B�B�B�;B��B�KB+QB9XB2aB3�BC{BL�B:�BB1B-�BE�BQ BcTB�B�VB�/B��B�B�B��B�vBԯB�B��B�1B҉B��B��B��B��Bu�B�MB��B��B�SB��B�B�zB�WB��B��B��B�dB��B�SBHBz�Bp�Bc�B[�BW�BM�B;0B B�B�B�BB�B�jB��B�B��B�nB�XB{By�Bx�Bp�Bh�BT�BC�B.�B�B<B
�qB
�eB
�5B
�gB
�.B
� B
��B
�B
�B
k�B
YB
BuB
3�B
'B
EB
:B
	�B
 �B	�qB	��B	�$B	�B	�;B	�fB	�FB	�)B	�B	�rB	��B	~BB	u�B	r�B	p�B	gRB	Z�B	L�B	C�B	=�B	;B	/�B	*�B	#�B	�B	SB	�B	�B	B	'B��B��B��B��B��B��B�B�B�vB�aB�HB��BĶB�4B�(B��B�(B��B��B��B�"B��B��B��B��B�	B��B�[B�B��B��B�?B�mB��B�;B�B��B��B��B��B�OB~]B}By	BxRB|�B�B�_B��B��B�]B�qB��B�hB��B�B�UB�lBvzBxlB�B|�BwfB��B��B�[B��B�!B��B��B��B�;B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�tB�ZB�FB�`B��B��B��B��B�^B�B��B��B�'B�MB�+BʌB�vB�}B҉BյB��B�B��B��B�6B�UB�?B�>B�"B	 iB	{B	�B		�B		�B		B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	/B	#B	&LB	+B	,"B	,"B	,=B	-]B	.}B	2|B	4�B	7�B	<�B	<�B	<�B	?�B	C�B	F�B	J�B	NB	OBB	VSB	ZQB	\]B	]~B	`�B	a�B	b�B	b�B	b�B	d�B	e�B	e�B	i�B	j�B	k�B	l�B	m�B	q�B	r�B	s�B	v+B	x8B	}<B	HB	�iB	�uB	�GB	��B	��B	�zB	�XB	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�5B	�5B	��B	��B	�B	�B	��B	�B	�$B	�DB	�KB	�WB	�)B	�)B	�/B	�cB	��B	��B	�aB	�hB	��B	�tB	�ZB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	�B	��B	��B	� B	�B	� B	� B	� B	��B	�KB	�=B	�5B	ߊB	�B	�B	�\B	�hB	�nB	�ZB	�fB	�sB	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	�B	�.B	�.B
 4B
 OB
;B
 4B
UB
;B
 B
 B
 B
 B
UB
;B
AB
GB
{B
oB
UB
UB
oB
;B
GB
-B
GB
EB
EB
EB
fB
KB
KB
fB
	lB
	RB
	lB

�B
�B
�B
�B
~B
dB
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
}B
}B
}B
}B
}B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
	B
#B
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
�B
B
 'B
 'B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#B
$&B
$B
#�B
#�B
$B
%,B
%B
&2B
&LB
'B
'8B
(>B
($B
($B
($B
)DB
)_B
*KB
*KB
*KB
+6B
+6B
+B
+B
+B
+6B
+QB
,=B
,"B
,"B
,"B
,=B
,WB
-]B
-)B
-CB
.IB
./B
.IB
/OB
/5B
/5B
0;B
0;B
0;B
0oB
0oB
1[B
1AB
1AB
1AB
1[B
2aB
2GB
2GB
3hB
3MB
3MB
3MB
3MB
3hB
3hB
4nB
4TB
5tB
5ZB
5?B
5tB
5ZB
5tB
5tB
6�B
6`B
6`B
6`B
7fB
7fB
7�B
8lB
8�B
9rB
9rB
9rB
9�B
9rB
9rB
9rB
:xB
:xB
:xB
:�B
:�B
;�B
;�B
;�B
;�B
;�B
<�B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
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
B�B
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
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
G�B
G�B
H�B
H�B
H�B
IB
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
J�B
J�B
J�B
K�B
MB
MB
MB
L�B
MB
M�B
M�B
M�B
NB
M�B
M�B
M�B
NB
M�B
NB
OB
N�B
OB
N�B
OB
N�B
N�B
N�B
PB
PB
O�B
Q B
Q B
QB
Q B
P�B
Q B
Q B
Q B
Q B
Q B
QB
Q B
Q B
QB
RTB
S@B
TB
TB
TB
T,B
U2B
U2B
V9B
V9B
V9B
W$B
W$B
W?B
X+B
X_B
Y1B
YB
Y1B
YKB
YKB
Y1B
Z7B
ZB
ZB
Z7B
ZQB
Z7B
[=B
[WB
\CB
\]B
\]B
\]B
\CB
\CB
]IB
]/B
]/B
]IB
]IB
]IB
]IB
]IB
]/B
]IB
^5B
^OB
^OB
^OB
^�B
_pB
_pB
_VB
_VB
`\B
`\B
`BB
`BB
`\B
`BB
`BB
`\B
`\B
aHB
abB
abB
a|B
a|B
a�B
bhB
bNB
bhB
bhB
cnB
cTB
cnB
cnB
c�B
cnB
c�B
dtB
dtB
dtB
dtB
dtB
d�B
ezB
ezB
ezB
e�B
f�B
ffB
f�B
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
i�B
i�B
iyB
iyB
i�B
i�B
i�B
i�B
jB
j�B
i�B
j�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
p�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
tB
s�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
xB
xB
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609250034002016092500340020160925003400201806221302282018062213022820180622130228201804050701532018040507015320180405070153  JA  ARFMdecpA19c                                                                20160921063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160920213519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160920213520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160920213520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160920213521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160920213521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160920213521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160920213521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160920213521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160920213522                      G�O�G�O�G�O�                JA  ARUP                                                                        20160920223108                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160921153526  CV  JULD            G�O�G�O�F�c�                JM  ARCAJMQC2.0                                                                 20160924153400  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160924153400  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220153  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040228  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                