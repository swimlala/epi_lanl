CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-11T00:35:24Z creation;2018-10-11T00:35:29Z conversion to V3.1;2019-12-19T07:26:50Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181011003524  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              "A   JA  I2_0577_290                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؇�� 1   @؇���J @4u\(��d[��w�k1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
=@}p�@��R@��RA\)A?\)A_\)A�z�A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca�)Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�
D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�{�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�D���D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D�θ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AمAه+AكA�~�A�x�A�z�A�ZA�I�A���A�G�A��#Aכ�A׉7A�x�A�\)A�5?A���A�E�Aմ9A��A��A԰!AԃA�;dAӬAѩ�A��yA�M�A̝�Aʉ7AȼjA��
A�JA�G�A���A�A�$�A�bA��hA�A�A��;A�ĜA�dZA�\)A��FA�%A���A��FA��A��!A�  A��7A��wA��^A�{A�ȴA��+A�
=A�$�A�(�A�?}A��hA�"�A�33A�~�A���A�;dA���A�?}A�  A�33A�(�A�1'A�ZA��wA���A�^5A�O�A�%A�%A�1A�|�A�A���A�A�33A���A���A��hA��A�l�A�S�Al�A~�AxAvȴAt�Ap��AooAl��Al  AkAj�9Aj�Ag�Af9XAdA�A`�\A_`BA_A^VA]l�A[�TAX�AT-AS%AQ+AO&�AN(�AL��AK��AKVAJA�AH�DAGoAF$�AD�AAhsA=�FA;��A:�A:�jA:ffA9C�A7��A77LA6��A6E�A4��A2��A1�TA0��A/�mA.Q�A,�`A+K�A)�A(bNA'�FA&��A%ƨA$1'A"9XA ȴA ZA\)A��A5?A�
A�A��Ax�A�A�uA�A��AƨAoAI�AA|�A��AC�A�A�DAE�A��A��A�TAK�A�A|�A��A��Av�A�A
JA	�AI�A�hA��A-A`BAG�A
=A��AG�An�A��A�/A��A ��A 9X@�
=@��T@�hs@�&�@���@�Z@�S�@��y@��w@��@�33@��-@�(�@�S�@�E�@�7L@�@�V@�S�@�ff@�D@�ƨ@�R@�j@�r�@��T@��@㝲@�7@�Z@���@�-@�7L@�9X@�=q@ّh@�Q�@��@��@��;@���@Ѳ-@ѡ�@��@��@�K�@�`B@��m@ə�@�X@���@ȣ�@���@ȋD@�I�@�1@Ǿw@ȓu@�hs@�o@�M�@��@��@��@���@�Q�@�ƨ@�@��R@���@���@�V@�5?@�G�@�`B@���@���@�^5@��R@�ff@�ƨ@���@��;@��H@���@��@�-@��@�V@���@���@�bN@�\)@�;d@��@�M�@��@���@��@���@�z�@�1'@��@���@��@��!@�ff@��@�x�@��@�Ĝ@���@�Z@��
@�l�@��@�dZ@�
=@��\@��R@���@��@�E�@��@���@�Z@��F@�K�@��@��@��H@���@�E�@���@�/@���@��j@��@�r�@��m@���@��@��
@�ƨ@��w@�|�@��@��y@�ȴ@���@��+@�ff@�5?@�J@��-@��@��D@�Z@�A�@�9X@� �@��w@��P@�dZ@�K�@�+@��@��!@���@�V@��@��T@��^@��7@�x�@�O�@��j@�r�@�Q�@�I�@�I�@�(�@�  @�ƨ@��@�+@��!@�n�@�E�@�J@���@���@��@��9@��@��w@��@�C�@�
=@��+@�$�@���@��@���@�x�@��@���@�r�@�A�@��@��;@�ƨ@��@�dZ@��@��H@���@�v�@�5?@��#@���@�G�@���@��`@���@���@�1'@�b@�  @�ƨ@��P@�t�@�|�@�S�@��H@��\@�5?@��#@��7@�X@��@��@��/@�z�@�(�@�1'@�(�@�|�@�+@��@�
=@��y@���@�ȴ@���@��\@�V@�5?@���@�x�@�&�@���@�Q�@�|�@��@�|�@�"�@�o@�o@��y@��R@�v�@���@�M�@��T@��T@��@��^@�`B@�7L@�/@�%@���@�r�@�bN@�Z@�9X@��@���@��@�;d@�@�E�@�@���@��#@��#@���@���@���@�p�@�O�@�&�@��@�bN@� �@�b@�  @|�@~��@}�T@}`B@|�@|I�@{�F@{"�@z�H@z��@z-@y�#@y��@x�9@x�@xb@x  @w�;@w�w@w|�@v�@v$�@u@up�@u?}@t�@t�j@t9X@t�@s�@so@r�H@r��@r-@q�7@p�9@pbN@p  @o��@o
=@nff@mO�@l�/@l�@l�D@l1@kƨ@k�@kS�@j��@jM�@j�@i��@i�#@i�@i�@i�#@i�^@i7L@h��@h��@h�9@hr�@h1'@g+@f�y@g+@f��@f�@f�R@f�+@fff@f$�@e�T@e�h@e/@d��@d��@d��@c�m@cdZ@c"�@b�\@a��@a�^@a��@a7L@`Ĝ@`1'@`  @_��@_\)@_+@^��@^�R@^��@^�+@^v�@^V@^$�@]�@\��@\��@\�@\�D@\I�@\9X@\(�@[ƨ@[�@[@Z��@Z��@Z~�@Z�@Yhs@X��@X��@XbN@X �@W�@W��@W+@V�R@VV@U�T@U�h@U�@UV@T�/@T�j@T�D@S�m@S�@S33@So@R��@Rn�@R^5@RJ@Q�^@Qhs@Q7L@Q&�@P�`@PĜ@P�@P1'@O�@Ol�@O
=@Nȴ@N�+@NV@N5?@M��@M�@M/@L�/@L��@Lz�@LZ@LI�@K�m@Kƨ@K��@Kt�@J�!@J^5@JJ@I��@Ihs@IG�@I7L@I&�@H�9@H1'@G�@G�P@G\)@GK�@F�y@F�+@Fv�@F{@E`B@E�@E/@EV@D��@DI�@C�m@C��@C�@C�@Co@B�\@Bn�@A�#@AX@@�9@@1'@?�P@?\)@?
=@>�y@>ȴ@>��@>�+@>V@>$�@=�T@=��@=@=�@=p�@=`B@=`B@=p�@=`B@<�@<j@<I�@<�@;�m@;��@;33@:�\@:�@:J@9�@9�#@9�^@9�7@9G�@9%@8��@8�`@8Ĝ@8�u@8A�@8 �@8  @7�w@7��@7|�@7�@7|�@7K�@7+@7
=@7
=@6�y@6�y@6V@6E�@6$�@5�@5�T@5��@5�@5�@4��@41@3C�@2�@2��@2��@2~�@2-@1��@1�@1�^@1%@0Ĝ@0�9@0��@0�@01'@/�;@/l�@/K�@.��@.�y@.�@.ȴ@.�R@.�R@.��@.�+@.V@.@-@-��@-O�@-�@,�/@,��@,��@,�D@,9X@,1@+��@+�
@+ƨ@+�F@+t�@+o@*��@*�!@*��@*�\@*n�@*-@*J@)�^@)x�@)X@)�@(�`@(Ĝ@(A�@(b@'�@'�@'�;@'�@'\)@&��@&�@&��@&v�@&v�@&V@&{@%�T@%�-@%�@%/@%V@$�@$�@$j@$Z@$9X@$�@$1@#��@#�F@#��@#S�@#@"�!@"M�@!�#@!�^@!��@!hs@!&�@ ��@ Ĝ@ �@ bN@  �@�@�@\)@+@
=@�R@5?@�@�T@@`B@?}@�@�@��@��@z�@Z@9X@(�@��@�F@S�@C�@o@��@��@^5@-@�@�^@��@G�@&�@��@r�@ �@�@\)@;d@�y@��@v�@v�@v�@ff@ff@5?@�T@��@p�@O�@/@�@�@Z@9X@(�@��@��@�
@�F@��@dZ@"�@@�H@��@��@n�@-@J@�@��@�7@x�@hs@X@7L@&�@&�@%@Ĝ@�@r�@bN@Q�@1'@  @�@�P@|�@|�@|�@\)@;d@
=@��@�y@�@ȴ@�R@�+@V@E�@5?@@�T@��@@@�@?}@/@V@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AمAه+AكA�~�A�x�A�z�A�ZA�I�A���A�G�A��#Aכ�A׉7A�x�A�\)A�5?A���A�E�Aմ9A��A��A԰!AԃA�;dAӬAѩ�A��yA�M�A̝�Aʉ7AȼjA��
A�JA�G�A���A�A�$�A�bA��hA�A�A��;A�ĜA�dZA�\)A��FA�%A���A��FA��A��!A�  A��7A��wA��^A�{A�ȴA��+A�
=A�$�A�(�A�?}A��hA�"�A�33A�~�A���A�;dA���A�?}A�  A�33A�(�A�1'A�ZA��wA���A�^5A�O�A�%A�%A�1A�|�A�A���A�A�33A���A���A��hA��A�l�A�S�Al�A~�AxAvȴAt�Ap��AooAl��Al  AkAj�9Aj�Ag�Af9XAdA�A`�\A_`BA_A^VA]l�A[�TAX�AT-AS%AQ+AO&�AN(�AL��AK��AKVAJA�AH�DAGoAF$�AD�AAhsA=�FA;��A:�A:�jA:ffA9C�A7��A77LA6��A6E�A4��A2��A1�TA0��A/�mA.Q�A,�`A+K�A)�A(bNA'�FA&��A%ƨA$1'A"9XA ȴA ZA\)A��A5?A�
A�A��Ax�A�A�uA�A��AƨAoAI�AA|�A��AC�A�A�DAE�A��A��A�TAK�A�A|�A��A��Av�A�A
JA	�AI�A�hA��A-A`BAG�A
=A��AG�An�A��A�/A��A ��A 9X@�
=@��T@�hs@�&�@���@�Z@�S�@��y@��w@��@�33@��-@�(�@�S�@�E�@�7L@�@�V@�S�@�ff@�D@�ƨ@�R@�j@�r�@��T@��@㝲@�7@�Z@���@�-@�7L@�9X@�=q@ّh@�Q�@��@��@��;@���@Ѳ-@ѡ�@��@��@�K�@�`B@��m@ə�@�X@���@ȣ�@���@ȋD@�I�@�1@Ǿw@ȓu@�hs@�o@�M�@��@��@��@���@�Q�@�ƨ@�@��R@���@���@�V@�5?@�G�@�`B@���@���@�^5@��R@�ff@�ƨ@���@��;@��H@���@��@�-@��@�V@���@���@�bN@�\)@�;d@��@�M�@��@���@��@���@�z�@�1'@��@���@��@��!@�ff@��@�x�@��@�Ĝ@���@�Z@��
@�l�@��@�dZ@�
=@��\@��R@���@��@�E�@��@���@�Z@��F@�K�@��@��@��H@���@�E�@���@�/@���@��j@��@�r�@��m@���@��@��
@�ƨ@��w@�|�@��@��y@�ȴ@���@��+@�ff@�5?@�J@��-@��@��D@�Z@�A�@�9X@� �@��w@��P@�dZ@�K�@�+@��@��!@���@�V@��@��T@��^@��7@�x�@�O�@��j@�r�@�Q�@�I�@�I�@�(�@�  @�ƨ@��@�+@��!@�n�@�E�@�J@���@���@��@��9@��@��w@��@�C�@�
=@��+@�$�@���@��@���@�x�@��@���@�r�@�A�@��@��;@�ƨ@��@�dZ@��@��H@���@�v�@�5?@��#@���@�G�@���@��`@���@���@�1'@�b@�  @�ƨ@��P@�t�@�|�@�S�@��H@��\@�5?@��#@��7@�X@��@��@��/@�z�@�(�@�1'@�(�@�|�@�+@��@�
=@��y@���@�ȴ@���@��\@�V@�5?@���@�x�@�&�@���@�Q�@�|�@��@�|�@�"�@�o@�o@��y@��R@�v�@���@�M�@��T@��T@��@��^@�`B@�7L@�/@�%@���@�r�@�bN@�Z@�9X@��@���@��@�;d@�@�E�@�@���@��#@��#@���@���@���@�p�@�O�@�&�@��@�bN@� �@�b@�  @|�@~��@}�T@}`B@|�@|I�@{�F@{"�@z�H@z��@z-@y�#@y��@x�9@x�@xb@x  @w�;@w�w@w|�@v�@v$�@u@up�@u?}@t�@t�j@t9X@t�@s�@so@r�H@r��@r-@q�7@p�9@pbN@p  @o��@o
=@nff@mO�@l�/@l�@l�D@l1@kƨ@k�@kS�@j��@jM�@j�@i��@i�#@i�@i�@i�#@i�^@i7L@h��@h��@h�9@hr�@h1'@g+@f�y@g+@f��@f�@f�R@f�+@fff@f$�@e�T@e�h@e/@d��@d��@d��@c�m@cdZ@c"�@b�\@a��@a�^@a��@a7L@`Ĝ@`1'@`  @_��@_\)@_+@^��@^�R@^��@^�+@^v�@^V@^$�@]�@\��@\��@\�@\�D@\I�@\9X@\(�@[ƨ@[�@[@Z��@Z��@Z~�@Z�@Yhs@X��@X��@XbN@X �@W�@W��@W+@V�R@VV@U�T@U�h@U�@UV@T�/@T�j@T�D@S�m@S�@S33@So@R��@Rn�@R^5@RJ@Q�^@Qhs@Q7L@Q&�@P�`@PĜ@P�@P1'@O�@Ol�@O
=@Nȴ@N�+@NV@N5?@M��@M�@M/@L�/@L��@Lz�@LZ@LI�@K�m@Kƨ@K��@Kt�@J�!@J^5@JJ@I��@Ihs@IG�@I7L@I&�@H�9@H1'@G�@G�P@G\)@GK�@F�y@F�+@Fv�@F{@E`B@E�@E/@EV@D��@DI�@C�m@C��@C�@C�@Co@B�\@Bn�@A�#@AX@@�9@@1'@?�P@?\)@?
=@>�y@>ȴ@>��@>�+@>V@>$�@=�T@=��@=@=�@=p�@=`B@=`B@=p�@=`B@<�@<j@<I�@<�@;�m@;��@;33@:�\@:�@:J@9�@9�#@9�^@9�7@9G�@9%@8��@8�`@8Ĝ@8�u@8A�@8 �@8  @7�w@7��@7|�@7�@7|�@7K�@7+@7
=@7
=@6�y@6�y@6V@6E�@6$�@5�@5�T@5��@5�@5�@4��@41@3C�@2�@2��@2��@2~�@2-@1��@1�@1�^@1%@0Ĝ@0�9@0��@0�@01'@/�;@/l�@/K�@.��@.�y@.�@.ȴ@.�R@.�R@.��@.�+@.V@.@-@-��@-O�@-�@,�/@,��@,��@,�D@,9X@,1@+��@+�
@+ƨ@+�F@+t�@+o@*��@*�!@*��@*�\@*n�@*-@*J@)�^@)x�@)X@)�@(�`@(Ĝ@(A�@(b@'�@'�@'�;@'�@'\)@&��@&�@&��@&v�@&v�@&V@&{@%�T@%�-@%�@%/@%V@$�@$�@$j@$Z@$9X@$�@$1@#��@#�F@#��@#S�@#@"�!@"M�@!�#@!�^@!��@!hs@!&�@ ��@ Ĝ@ �@ bN@  �@�@�@\)@+@
=@�R@5?@�@�T@@`B@?}@�@�@��@��@z�@Z@9X@(�@��@�F@S�@C�@o@��@��@^5@-@�@�^@��@G�@&�@��@r�@ �@�@\)@;d@�y@��@v�@v�@v�@ff@ff@5?@�T@��@p�@O�@/@�@�@Z@9X@(�@��@��@�
@�F@��@dZ@"�@@�H@��@��@n�@-@J@�@��@�7@x�@hs@X@7L@&�@&�@%@Ĝ@�@r�@bN@Q�@1'@  @�@�P@|�@|�@|�@\)@;d@
=@��@�y@�@ȴ@�R@�+@V@E�@5?@@�T@��@@@�@?}@/@V@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B=qB<jB<jB<jB<jB;dBI�BgmBk�Bw�B�B�PB�uB��B��B�'BɺBVB7LBXBYB\)BVBJ�B)�B�BoBbB��BB�B�B\BXBS�B2-B.BJ�Bq�Bm�B]/B^5Bm�B|�Bz�Br�By�B�B�VB�B�By�B_;Bm�B|�Bu�Bl�BW
BI�B?}B)�B(�B2-B7LB0!B!�BB�B�B�`B��B�=B�%B� BcTB6FB$�B0!B�B�B	7B
��B
��B
�B
\)B
^5B
R�B
F�B
%�B
�B
JB	�fB	�/B	��B	��B	��B	y�B	�\B	�B	�%B	�B	�PB	�B	l�B	jB	`BB	=qB	O�B	ZB	N�B	;dB	!�B�B�BB��B�B�fB��B�B��B�B�yB�B��B��B�wB��B�+B��B�-B��B�dB�!B��B�3B�-B��B��B�JB�bB�oB�PB�7B~�B� Bz�By�B�Bz�Bw�Bk�BdZBdZBw�Bn�Bt�Bl�Bk�BcTBYBe`BdZB_;BYBP�BO�BM�BN�BM�BO�BD�B<jBL�BL�BJ�BD�B<jB<jB=qB;dB8RB@�BC�B>wB,B2-B0!B7LB8RB<jBC�B@�BN�BP�B[#Br�BjBv�Bm�Be`BcTBr�Bn�Bp�Bw�B{�B}�B�1B�%B�7B��B��B��B�bB�VB�uB�PB�Bp�BXBr�B{�Br�B~�B�B�B�DB��B��B�B}�B�%B�B�7B�+B�%B�B�1B�+B� B�%B� B�JB�oB��B��B��B��B��B��B��B�B�B�!B�3B�-B�-B�3B�?B��B�B��B�^B�wB�}B��BĜBBƨBŢB��B�B�B�;B�TB�ZB�B�B��B��B��B�B�NB�BB��B�fB�/B�B�;B�TB�fB�yB�yB�yB�`B�B�B�B�B��B	B	B	B	B	B	B	B	DB	PB	PB	hB	�B	�B	�B	�B	�B	#�B	+B	.B	33B	0!B	9XB	:^B	=qB	:^B	>wB	>wB	8RB	>wB	E�B	H�B	K�B	N�B	N�B	N�B	Q�B	S�B	]/B	bNB	bNB	bNB	aHB	iyB	l�B	n�B	o�B	p�B	n�B	o�B	t�B	v�B	x�B	x�B	y�B	y�B	y�B	y�B	x�B	{�B	�B	�B	�%B	�%B	�B	�1B	�DB	�JB	�JB	�PB	�bB	�hB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�B	�9B	�9B	�XB	�dB	�jB	�jB	�jB	��B	ĜB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�/B	�5B	�/B	�#B	�#B	�/B	�5B	�BB	�TB	�TB	�ZB	�TB	�HB	�NB	�mB	�mB	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
1B
%B
B
B
+B
+B
+B
B
1B

=B

=B
	7B
1B
1B
JB
JB
VB
VB
VB
hB
hB
hB
hB
hB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
"�B
#�B
&�B
&�B
%�B
$�B
"�B
%�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
+B
)�B
(�B
)�B
,B
+B
+B
-B
.B
-B
,B
,B
.B
.B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
1'B
1'B
/B
1'B
33B
49B
49B
33B
49B
49B
33B
33B
33B
49B
5?B
49B
33B
2-B
49B
5?B
5?B
6FB
6FB
5?B
5?B
5?B
6FB
6FB
7LB
6FB
8RB
8RB
8RB
7LB
5?B
7LB
8RB
9XB
8RB
8RB
:^B
9XB
9XB
:^B
:^B
;dB
;dB
;dB
:^B
:^B
;dB
:^B
;dB
<jB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
?}B
@�B
@�B
?}B
=qB
@�B
A�B
A�B
B�B
C�B
C�B
B�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
B�B
D�B
C�B
A�B
D�B
F�B
E�B
E�B
C�B
D�B
E�B
F�B
E�B
D�B
C�B
D�B
B�B
A�B
A�B
A�B
A�B
D�B
D�B
E�B
E�B
F�B
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
G�B
E�B
E�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
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
O�B
P�B
Q�B
P�B
Q�B
Q�B
O�B
O�B
P�B
O�B
N�B
Q�B
S�B
T�B
S�B
S�B
T�B
VB
T�B
S�B
VB
XB
XB
XB
W
B
W
B
W
B
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
YB
YB
ZB
ZB
[#B
[#B
\)B
\)B
[#B
[#B
\)B
]/B
\)B
]/B
\)B
\)B
[#B
]/B
]/B
^5B
^5B
^5B
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
`BB
`BB
`BB
_;B
_;B
^5B
`BB
`BB
aHB
aHB
aHB
`BB
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
dZB
cTB
bNB
cTB
bNB
bNB
bNB
bNB
cTB
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
gmB
ffB
ffB
gmB
hsB
hsB
gmB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
jB
jB
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
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
l�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
q�B
r�B
q�B
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
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
x�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B=qB<�B<jB<�B<�B;�BJ#Bh$Bl�Bx�B��B��B��B�B�RB�B�)BBB88BX+BY�B\�BW
BL�B.IBWBBB��B	�B�GB�*B{BXyBUgB6+B2|BM�BraBn�B_�Ba-Bo�B~�B|�ButB|B��B�(B��B�uB|Bb�Bn�B}qBv�Bm�BYKBL0BA�B-�B+�B49B8�B1�B#�B+B�B�B�B��B�VB�1B��Bf�B;�B)yB2�B 'B�B
�B
�qB
�oB
�vG�O�B
a�B
U�B
I�B
)�B
B
B	�eB	��B	��B	�B	��B	~(B	��B	�aB	�EB	�SB	��B	�uB	o�B	lqB	b�B	A B	QB	Z�B	O�B	="B	$�B�fB��B�fB�B��B�	B�B��B��B��B�]B��B�vB��B�#B��B�B��B��B�6B��B��B��B��B�B��B��B�B��B��B�xB� B�B|�B{�B�-B|jBy>Bm�Bf�Bf2BxRBpButBm�Bl=BdtBZ�Be�BeB`'BZ7BR�BQ4BOBO�BN�BP�BFB>(BM6BMjBK^BE�B=�B=�B>wB<�B9�BA;BD3B?HB./B3�B1�B8lB9rB=�BDgBA�BOBQ4B[#Bs3Bk�Bw�Bo BgBd�BsMBo�BqvBxB|6B~wB��B��B�lB�2B��B�	B��B�\B�B�<B�MBr�B[qBs�B|�BtB�B��B�3B�^B�
B�$B��B}B�B�B��B��B�+B�[B��B�1B� B�B�UB�B�@B��B�tB��B��B�#B��B��B�)B�wB�;B�3B��B��B��B��B��B�UB��B��B��B��B��B��B�B�EB�?B�B��BچBߊB�B��B�B�B��B��B��B�[B��B�B�B�mB޸B�WB��B��B�B��B��B��B�2B��B�B�B�oB�(B	-B	MB	aB	[B	aB	gB	�B	xB	�B	�B	�B	�B	�B	�B	�B	B	$B	+B	.cB	3hB	0�B	9>B	:xB	=�B	:�B	>�B	>�B	9$B	>�B	E�B	H�B	K�B	OB	OB	OBB	RoB	TaB	]dB	bhB	bhB	b�B	a�B	i�B	l�B	n�B	o�B	p�B	n�B	o�B	t�B	v�B	x�B	y	B	y�B	zB	zB	zDB	yXB	|6B	�;B	�-B	�?B	�YB	�mB	�fB	�^B	�dB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	�B	�B	�B	�B	�*B	�DB	�DB	�DB	�KB	�WB	�OB	�UB	�UB	�AB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	�B	��B	�	B	�)B	�B	�"B	�B	��B	�B	�&B	� B	� B	� B	�B	�&B	�:B	�2B	�MB	�9B	�1B	�1B	�_B	�mB	�7B	�=B	�QB	�WB	�IB	�OB	�dB	�qB	�qB	�~B	ބB	�vB	�B	�B	�tB	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�>B	�B	�B	�B	�B	�BB	�PB	�B	�.B	�HB	�qB
 OB
B
3B
B
B
YB
mB
SB
EB
zB
_B
�B
fB

rB

rB
	lB
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
B
�B
�B
 �B
B
 �B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
#B
$B
'B
'B
%�B
%B
#:B
%�B
(�B
(
B
)B
)B
)B
)B
)B
)B
)B
)DB
+B
+B
*0B
)DB
*0B
,=B
+QB
+QB
-CB
./B
-]B
,WB
,=B
./B
.IB
0UB
0;B
0;B
1AB
2-B
2aB
2-B
1[B
1AB
/iB
1[B
3hB
4TB
4nB
3MB
49B
4TB
3�B
3MB
3hB
4TB
5ZB
4TB
3hB
2|B
4�B
5ZB
5ZB
6`B
6`B
5�B
5tB
5tB
6zB
6�B
7fB
6`B
8RB
8lB
8lB
7fB
5�B
7fB
8lB
9�B
8lB
8lB
:xB
9rB
9rB
:xB
:xB
;B
;B
;�B
:xB
:xB
;B
:�B
;B
<�B
=�B
=�B
>�B
=�B
=�B
>�B
>�B
?�B
@�B
@�B
@�B
?�B
@�B
@�B
?�B
=�B
@�B
A�B
A�B
B�B
C�B
C�B
B�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
B�B
D�B
C�B
A�B
D�B
F�B
E�B
E�B
C�B
D�B
E�B
F�B
E�B
D�B
C�B
D�B
B�B
A�B
A�B
A�B
A�B
D�B
D�B
E�B
E�B
F�B
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
G�B
E�B
E�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K�B
J�B
J�B
J�B
J�B
K�B
MB
L�B
M�B
M�B
N�B
N�B
N�B
PB
O�B
P�B
QB
Q B
PB
Q B
Q�B
Q B
RB
RB
PB
P.B
Q4B
P.B
OBB
R B
T,B
U2B
TB
TB
UB
VB
UB
TFB
V9B
XB
X+B
X+B
W$B
W$B
W?B
Y1B
Y1B
ZB
ZB
ZB
ZB
ZB
Z7B
ZQB
Y1B
Y1B
Y1B
Z7B
ZQB
[=B
[=B
\)B
\)B
[=B
[WB
\CB
]IB
\CB
]IB
\CB
\CB
[=B
]dB
]IB
^5B
^OB
^OB
]IB
]IB
]IB
]dB
^OB
^OB
^OB
^OB
^jB
_VB
`BB
`BB
`\B
_pB
_pB
^jB
`\B
`\B
aHB
aHB
abB
`vB
abB
abB
abB
abB
b�B
bhB
bhB
bhB
cTB
cnB
cnB
dZB
cnB
bhB
cnB
bhB
bhB
bhB
b�B
c�B
e`B
ezB
d�B
dtB
ezB
ezB
ezB
e�B
ezB
ezB
f�B
e�B
f�B
g�B
f�B
f�B
g�B
hsB
h�B
g�B
i�B
i�B
iyB
i�B
h�B
h�B
i�B
j�B
j�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
q�B
r�B
q�B
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
v�B
v�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
x�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810150038322018101500383220181015003832201810150200262018101502002620181015020026201810160022202018101600222020181016002220  JA  ARFMdecpA19c                                                                20181011093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181011003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181011003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181011003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181011003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181011003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181011003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181011003529  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181011003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181011003529  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181011003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181011003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20181011005530                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181011153556  CV  JULD            G�O�G�O�F�?�                JM  ARSQJMQC2.0                                                                 20181012000000  CF  PSAL_ADJUSTED_QCC(  C*  G�O�                JM  ARSQJMQC2.0                                                                 20181012000000  CF  TEMP_ADJUSTED_QCC(  C(  G�O�                JM  ARCAJMQC2.0                                                                 20181014153832  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181014153832  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181014170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181015152220  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                