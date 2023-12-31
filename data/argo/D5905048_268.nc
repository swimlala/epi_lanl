CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-06T00:37:00Z creation;2018-08-06T00:37:15Z conversion to V3.1;2019-12-19T07:31:52Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180806003700  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_268                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�wv
� 1   @�wv�5�@4���l��d`�䎊1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDH�DH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D���D���D�;�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�D���D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D��D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�z�A�r�A�t�A�x�A�hsA�ffA�ffA�dZA�bNA�bNA�ffA�bNA�\)A�S�A�M�A�K�A�oAܙ�AۋDA���A���A��A�Aѡ�AЬAβ-A��mA�1'A�`BA��;AʑhA�x�Aɗ�A�=qA�x�A�%A��#A��A�JA�E�A��PA���A���A��uA�bA��A�1'A���A�1A���A�t�A�^5A�K�A���A�XA��A��A���A�\)A� �A��jA��7A�$�A��yA�=qA��yA��RA�VA��!A�E�A�{A�I�A�t�A�1'A�dZA��A�x�A�K�A�ZA�^5A���A� �A�A��RA��A�r�A���A���A�K�A�Q�A�  A�`BA�-A���A�{A��A�t�A�%A���A��A��!A��/A���A���A��-A�+A��-A�bA�S�A��wA�E�A�E�A�r�A��/A�S�A�\)A�&�A��A�v�A�C�A�FA|�Aw��At��AtAr�Ao��Al��AiAg`BAe�AbjA`�A_�A^��A\��A\VAZ�AXffAW�PAU��AS�wARbAP(�AO|�AN��AM��AM?}AL9XAJ�uAI?}AHE�AGhsAEƨAB��A@��A?��A>��A<�/A:=qA9��A7�mA6�uA5O�A4��A3�#A333A2{A/�#A-��A,��A+��A)G�A(-A'�-A'hsA&�A%l�A#�A#p�A"�uA!�A =qA1'A��A�AVA��A�A;dA1A��Al�A��A�A�PA��A��Ax�A1'AVAffA�wA`BA
��A
Q�A	l�A��A�TA�/A�-AhsA
=A��A�^A�uA\)A =q@���@�O�@��@��@���@���@��/@�K�@��@�x�@��@�@���@�/@�9X@��@�+@��T@�V@��
@�|�@�o@�n�@��@�`B@���@�F@�+@�R@�E�@�@�ȴ@�?}@ߍP@��@��#@�O�@�bN@���@�n�@أ�@�1@�S�@�@��`@�bN@��m@ҧ�@с@Ь@��y@�5?@��`@�l�@ʸR@�M�@��@�  @�33@őh@Å@§�@��@�O�@�(�@��F@�S�@�\)@�C�@��H@�/@�Q�@�  @��w@�o@��\@�@��h@�Ĝ@��F@��@���@���@�&�@��j@��D@�1'@�l�@�@���@�$�@�@�G�@��@���@���@�I�@��@��m@��F@�t�@��R@��T@���@��@�G�@�V@��@���@�A�@��
@��
@�ƨ@��@�"�@��y@��!@�p�@��`@���@��@��@��9@���@��P@�33@���@�M�@��@�p�@�O�@�&�@���@��@�bN@�(�@���@��m@��;@��@�K�@�"�@��y@��!@�=q@���@�7L@�Ĝ@��@��u@�I�@��
@��@���@�dZ@�o@���@��+@�E�@��@�@���@�`B@�V@��/@��D@�(�@���@�t�@�
=@��y@��+@�5?@���@��7@��/@� �@��@�o@���@���@�dZ@�S�@�@���@��7@�O�@��7@��^@�hs@��9@�(�@��;@��m@��
@��w@�dZ@�"�@��@���@�-@���@��@���@�?}@��@���@�Q�@�b@�  @���@���@���@�l�@��@��R@�=q@��-@���@���@��h@�&�@���@�j@�1'@��F@��@��@�ȴ@���@�~�@�E�@�$�@���@��h@�p�@�`B@�`B@��@�r�@�9X@�b@��;@���@���@�K�@�C�@�"�@��@���@���@�E�@�J@��T@��@��-@��@�`B@��@�&�@��@���@���@��`@���@���@��@�bN@�Q�@� �@�  @��m@��F@�S�@�K�@�+@���@��\@�^5@�V@�-@��#@�x�@�G�@���@�Ĝ@�9X@�w@�P@\)@;d@~��@}�T@}/@|�D@|�j@|�/@|Z@|1@{�F@z�@z�!@z�\@z�!@z��@{33@{33@{o@{@z�\@z�@y�@y��@y7L@y%@y�@y&�@y&�@y�@y7L@zM�@y��@yG�@x�@x �@w�P@w
=@v��@vff@vE�@v@u�h@t�@tZ@s�m@s��@sdZ@s@r�!@q�@qx�@q%@p�`@p�9@pA�@o��@ol�@n�@nff@m��@mp�@mV@l��@l��@l��@l9X@k��@k�
@kƨ@k��@j�@j^5@i�@i&�@h�`@hĜ@h1'@h  @g|�@f�@fff@e��@eO�@eO�@e/@eV@d��@d(�@c�F@ct�@b�@bn�@b�@a�^@a&�@`�`@`��@`��@`�9@` �@_l�@_\)@_�@^��@^��@^ȴ@^5?@]��@]V@\�D@\z�@\j@\�@[�
@[�@[C�@[@Z�H@Z��@Z~�@Z�@Y��@Y%@X�u@XA�@W��@W�@W��@W\)@V��@Vȴ@V��@V��@VV@U�T@U��@U�@UO�@U/@T�/@T�D@Tj@T1@S�F@S�@SdZ@S"�@S@R�H@R�\@R^5@R-@Q��@Q�^@QX@Q�@P��@P�9@P�u@Pr�@PbN@P �@P  @O�P@O;d@N�@NV@N{@M��@M@M�@M?}@M�@L��@LI�@L9X@L�@K�m@Kƨ@K�F@K��@K"�@J�H@J�\@J=q@J=q@I��@I�^@I�7@Ihs@IG�@I&�@HĜ@HbN@H  @G�@G��@Gl�@G�@Fv�@F{@E�-@Ep�@D��@DI�@C�m@C��@C33@B��@B�\@B�@A�^@AX@A7L@A%@@Ĝ@@�u@@1'@?�;@?��@?l�@?\)@?+@>�@>��@>ff@>{@=�@=��@=�h@=O�@=/@<�@<�j@<�D@<j@<�@;�
@;S�@:��@:~�@:-@9��@9��@97L@8��@8�9@8�@8bN@8A�@8b@7��@7|�@7\)@6�y@6ȴ@6��@6�+@6�+@65?@5�-@5O�@5�@4��@4��@4Z@4(�@3�m@3ƨ@3��@3�@3S�@3C�@3C�@3"�@2�@2�@2��@2��@2~�@1��@1�#@1��@1&�@0��@0�`@0��@0Ĝ@0��@0�@0bN@0A�@0 �@0  @/�P@/�@/
=@.��@.v�@.E�@.@-�T@-�-@-�h@-`B@,��@,z�@,(�@+�
@+dZ@+33@*��@*�\@*^5@*-@*J@)��@)�7@)&�@(��@(�`@(��@(�u@(�u@(�@( �@'��@'��@'
=@&��@&�+@&$�@%�@%�-@%p�@%?}@%�@%�@%V@$��@$z�@$Z@$I�@$9X@#�m@#��@#C�@#33@#o@"�@"��@"�\@"=q@"-@"J@!�@!��@!��@!x�@!hs@!7L@!�@ �`@ Ĝ@ �u@ b@��@l�@K�@+@
=@�y@ȴ@v�@E�@5?@{@�-@��@�h@O�@V@�j@z�@z�@Z@��@��@t�@C�@33@"�@o@�@��@��@��@��@n�@J@��@�^@�^@�7@hs@X@7L@Ĝ@�u@bN@b@��@�w@�@�P@K�@+@ȴ@�R@��@��@��@��@v�@5?@�@��@@��@�@O�@�@��@�@��@��@�D@z�@Z@9X@�@�m@�F@��@t�@dZ@dZ@C�@33@"�@@��@��@~�@n�@^5@��@�@��@�^@��@x�@G�@��@�9@��@bN@1'@b@  @��@�@��@�P@|�@;d@�@
=@�y@�R@��@�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�z�A�r�A�t�A�x�A�hsA�ffA�ffA�dZA�bNA�bNA�ffA�bNA�\)A�S�A�M�A�K�A�oAܙ�AۋDA���A���A��A�Aѡ�AЬAβ-A��mA�1'A�`BA��;AʑhA�x�Aɗ�A�=qA�x�A�%A��#A��A�JA�E�A��PA���A���A��uA�bA��A�1'A���A�1A���A�t�A�^5A�K�A���A�XA��A��A���A�\)A� �A��jA��7A�$�A��yA�=qA��yA��RA�VA��!A�E�A�{A�I�A�t�A�1'A�dZA��A�x�A�K�A�ZA�^5A���A� �A�A��RA��A�r�A���A���A�K�A�Q�A�  A�`BA�-A���A�{A��A�t�A�%A���A��A��!A��/A���A���A��-A�+A��-A�bA�S�A��wA�E�A�E�A�r�A��/A�S�A�\)A�&�A��A�v�A�C�A�FA|�Aw��At��AtAr�Ao��Al��AiAg`BAe�AbjA`�A_�A^��A\��A\VAZ�AXffAW�PAU��AS�wARbAP(�AO|�AN��AM��AM?}AL9XAJ�uAI?}AHE�AGhsAEƨAB��A@��A?��A>��A<�/A:=qA9��A7�mA6�uA5O�A4��A3�#A333A2{A/�#A-��A,��A+��A)G�A(-A'�-A'hsA&�A%l�A#�A#p�A"�uA!�A =qA1'A��A�AVA��A�A;dA1A��Al�A��A�A�PA��A��Ax�A1'AVAffA�wA`BA
��A
Q�A	l�A��A�TA�/A�-AhsA
=A��A�^A�uA\)A =q@���@�O�@��@��@���@���@��/@�K�@��@�x�@��@�@���@�/@�9X@��@�+@��T@�V@��
@�|�@�o@�n�@��@�`B@���@�F@�+@�R@�E�@�@�ȴ@�?}@ߍP@��@��#@�O�@�bN@���@�n�@أ�@�1@�S�@�@��`@�bN@��m@ҧ�@с@Ь@��y@�5?@��`@�l�@ʸR@�M�@��@�  @�33@őh@Å@§�@��@�O�@�(�@��F@�S�@�\)@�C�@��H@�/@�Q�@�  @��w@�o@��\@�@��h@�Ĝ@��F@��@���@���@�&�@��j@��D@�1'@�l�@�@���@�$�@�@�G�@��@���@���@�I�@��@��m@��F@�t�@��R@��T@���@��@�G�@�V@��@���@�A�@��
@��
@�ƨ@��@�"�@��y@��!@�p�@��`@���@��@��@��9@���@��P@�33@���@�M�@��@�p�@�O�@�&�@���@��@�bN@�(�@���@��m@��;@��@�K�@�"�@��y@��!@�=q@���@�7L@�Ĝ@��@��u@�I�@��
@��@���@�dZ@�o@���@��+@�E�@��@�@���@�`B@�V@��/@��D@�(�@���@�t�@�
=@��y@��+@�5?@���@��7@��/@� �@��@�o@���@���@�dZ@�S�@�@���@��7@�O�@��7@��^@�hs@��9@�(�@��;@��m@��
@��w@�dZ@�"�@��@���@�-@���@��@���@�?}@��@���@�Q�@�b@�  @���@���@���@�l�@��@��R@�=q@��-@���@���@��h@�&�@���@�j@�1'@��F@��@��@�ȴ@���@�~�@�E�@�$�@���@��h@�p�@�`B@�`B@��@�r�@�9X@�b@��;@���@���@�K�@�C�@�"�@��@���@���@�E�@�J@��T@��@��-@��@�`B@��@�&�@��@���@���@��`@���@���@��@�bN@�Q�@� �@�  @��m@��F@�S�@�K�@�+@���@��\@�^5@�V@�-@��#@�x�@�G�@���@�Ĝ@�9X@�w@�P@\)@;d@~��@}�T@}/@|�D@|�j@|�/@|Z@|1@{�F@z�@z�!@z�\@z�!@z��@{33@{33@{o@{@z�\@z�@y�@y��@y7L@y%@y�@y&�@y&�@y�@y7L@zM�@y��@yG�@x�@x �@w�P@w
=@v��@vff@vE�@v@u�h@t�@tZ@s�m@s��@sdZ@s@r�!@q�@qx�@q%@p�`@p�9@pA�@o��@ol�@n�@nff@m��@mp�@mV@l��@l��@l��@l9X@k��@k�
@kƨ@k��@j�@j^5@i�@i&�@h�`@hĜ@h1'@h  @g|�@f�@fff@e��@eO�@eO�@e/@eV@d��@d(�@c�F@ct�@b�@bn�@b�@a�^@a&�@`�`@`��@`��@`�9@` �@_l�@_\)@_�@^��@^��@^ȴ@^5?@]��@]V@\�D@\z�@\j@\�@[�
@[�@[C�@[@Z�H@Z��@Z~�@Z�@Y��@Y%@X�u@XA�@W��@W�@W��@W\)@V��@Vȴ@V��@V��@VV@U�T@U��@U�@UO�@U/@T�/@T�D@Tj@T1@S�F@S�@SdZ@S"�@S@R�H@R�\@R^5@R-@Q��@Q�^@QX@Q�@P��@P�9@P�u@Pr�@PbN@P �@P  @O�P@O;d@N�@NV@N{@M��@M@M�@M?}@M�@L��@LI�@L9X@L�@K�m@Kƨ@K�F@K��@K"�@J�H@J�\@J=q@J=q@I��@I�^@I�7@Ihs@IG�@I&�@HĜ@HbN@H  @G�@G��@Gl�@G�@Fv�@F{@E�-@Ep�@D��@DI�@C�m@C��@C33@B��@B�\@B�@A�^@AX@A7L@A%@@Ĝ@@�u@@1'@?�;@?��@?l�@?\)@?+@>�@>��@>ff@>{@=�@=��@=�h@=O�@=/@<�@<�j@<�D@<j@<�@;�
@;S�@:��@:~�@:-@9��@9��@97L@8��@8�9@8�@8bN@8A�@8b@7��@7|�@7\)@6�y@6ȴ@6��@6�+@6�+@65?@5�-@5O�@5�@4��@4��@4Z@4(�@3�m@3ƨ@3��@3�@3S�@3C�@3C�@3"�@2�@2�@2��@2��@2~�@1��@1�#@1��@1&�@0��@0�`@0��@0Ĝ@0��@0�@0bN@0A�@0 �@0  @/�P@/�@/
=@.��@.v�@.E�@.@-�T@-�-@-�h@-`B@,��@,z�@,(�@+�
@+dZ@+33@*��@*�\@*^5@*-@*J@)��@)�7@)&�@(��@(�`@(��@(�u@(�u@(�@( �@'��@'��@'
=@&��@&�+@&$�@%�@%�-@%p�@%?}@%�@%�@%V@$��@$z�@$Z@$I�@$9X@#�m@#��@#C�@#33@#o@"�@"��@"�\@"=q@"-@"J@!�@!��@!��@!x�@!hs@!7L@!�@ �`@ Ĝ@ �u@ b@��@l�@K�@+@
=@�y@ȴ@v�@E�@5?@{@�-@��@�h@O�@V@�j@z�@z�@Z@��@��@t�@C�@33@"�@o@�@��@��@��@��@n�@J@��@�^@�^@�7@hs@X@7L@Ĝ@�u@bN@b@��@�w@�@�P@K�@+@ȴ@�R@��@��@��@��@v�@5?@�@��@@��@�@O�@�@��@�@��@��@�D@z�@Z@9X@�@�m@�F@��@t�@dZ@dZ@C�@33@"�@@��@��@~�@n�@^5@��@�@��@�^@��@x�@G�@��@�9@��@bN@1'@b@  @��@�@��@�P@|�@;d@�@
=@�y@�R@��@�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
B
B
  B	��B	�B	�;B	ƨB
#�B
^5B
Q�B
]/B
n�B
ȴB
�
B�BE�BZBaHBR�BZB�B�-B��B�B�)B�)B�mB�B��B"�B(�B1'B2-B2-B7LBF�B=qB2-B33BP�B`BBbNBZB@�BZBbNBaHBe`B]/BZBO�B8RB8RB9XB33B1BB	7B��B�B��BB  B�B��B�B��B��B��BB��B�B�
B�FB�^B�B��By�B��B�BbNBT�BdZB`BBXBH�B-B�BB
=BuB	7B
��B
�B
�;B
ÖB
��B
��B
�1B
S�B
+B
<jB
8RB
A�B
D�B
;dB
!�B	�B	ȴB	�wB	��B	�FB	�hB	v�B	e`B	t�B	aHB	\)B	]/B	cTB	_;B	H�B	T�B	@�B	)�B	.B	�B	bB	JB	+B	bB	bB	B	B��B�sB�mB�NB�/B��B�3B�^BŢBŢB�jB��B�FB��B��B��B��B��B�uB�Bp�BgmBw�Bl�B]/BhsBw�Bt�Bk�B[#B[#BffB`BBN�BXBE�BVBW
BVBA�BXB_;BXBffBhsB`BBZB`BBYBR�B`BBM�BA�BE�B`BBbNBbNB\)BXB]/BVBT�BS�B_;BbNB^5BS�BN�BI�BM�BL�B`BB_;B^5B^5B`BB_;B[#B_;BdZBaHB[#B`BBdZBcTBaHBl�BjBgmBgmBm�Bo�Bm�Bm�Bm�BgmBq�Bp�Bn�Bk�B`BBaHBcTBe`Bn�Bn�Bp�Bn�Bp�Bl�Bm�Bv�Bx�Br�Bu�B|�B{�Bt�Bu�B{�Bt�B�B� B�B�DB�PB�1B�=B�VB�=B�\B��B��B��B��B�3B�FB�dB�dB�FB�B�?B��B��B��BÖBÖBǮBǮBǮB��B��B��B�/B�BB�ZB�NB�TB�yB�B�B�B��B��B	B	B	B	%B	%B	%B	%B	B	DB	�B	�B	�B	�B	!�B	"�B	 �B	%�B	1'B	2-B	33B	8RB	=qB	>wB	9XB	?}B	J�B	J�B	J�B	H�B	H�B	Q�B	VB	XB	`BB	aHB	cTB	jB	jB	jB	n�B	o�B	p�B	s�B	u�B	u�B	s�B	r�B	v�B	w�B	w�B	v�B	x�B	~�B	�B	�+B	�1B	�+B	�1B	�VB	�\B	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�-B	�LB	�3B	�RB	�qB	��B	��B	�FB	�qB	ÖB	ÖB	ǮB	��B	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�
B	�#B	�#B	�B	�B	�B	�B	�
B	�B	�B	�5B	�/B	�)B	�B	�B	�)B	�)B	�B	�/B	�/B	�5B	�HB	�NB	�HB	�TB	�NB	�HB	�`B	�mB	�sB	�`B	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
	7B
DB

=B
DB
DB

=B
	7B
DB
DB
JB
\B
hB
hB
hB
oB
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
!�B
"�B
#�B
#�B
"�B
 �B
!�B
"�B
"�B
$�B
%�B
$�B
&�B
$�B
$�B
&�B
&�B
(�B
+B
+B
)�B
(�B
(�B
(�B
+B
)�B
)�B
,B
,B
,B
.B
/B
/B
.B
,B
+B
/B
/B
.B
/B
.B
,B
,B
.B
/B
2-B
2-B
1'B
1'B
2-B
1'B
2-B
33B
2-B
33B
2-B
1'B
1'B
2-B
49B
49B
6FB
6FB
5?B
5?B
6FB
6FB
6FB
6FB
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
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
;dB
;dB
<jB
;dB
;dB
;dB
:^B
:^B
:^B
:^B
<jB
<jB
=qB
=qB
=qB
=qB
<jB
=qB
?}B
>wB
>wB
>wB
?}B
>wB
=qB
>wB
?}B
?}B
A�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
?}B
@�B
A�B
B�B
A�B
@�B
?}B
A�B
A�B
A�B
A�B
A�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
H�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
K�B
K�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
J�B
K�B
I�B
J�B
K�B
L�B
L�B
M�B
L�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
N�B
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
T�B
T�B
T�B
S�B
T�B
S�B
T�B
R�B
R�B
T�B
T�B
S�B
W
B
W
B
XB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
VB
XB
W
B
XB
YB
YB
YB
YB
YB
XB
W
B
W
B
YB
YB
YB
[#B
ZB
\)B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
^5B
^5B
]/B
^5B
]/B
[#B
\)B
\)B
[#B
\)B
^5B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
`BB
`BB
bNB
bNB
bNB
aHB
aHB
aHB
cTB
cTB
cTB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
cTB
bNB
cTB
e`B
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
e`B
hsB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
gmB
gmB
iyB
iyB
jB
jB
jB
jB
jB
k�B
jB
jB
iyB
hsB
jB
k�B
k�B
jB
k�B
k�B
jB
iyB
jB
k�B
k�B
k�B
m�B
m�B
l�B
l�B
m�B
l�B
n�B
o�B
o�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
p�B
r�B
r�B
s�B
r�B
r�B
q�B
q�B
q�B
s�B
r�B
s�B
s�B
t�B
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
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
3B
3B
B
B
3B
B
B
B
%B
%B
%B
9B
9B
3B
9B
gB
 �B	��B	��B	�&B	�\B
&�B
^�B
T�B
_�B
r-B
�=B
ںB �BF�BZ�Ba�BU�B]~B��B�B�B�	B�B��B�B��B �B#�B*B2B3MB3hB8�BG+B>�B4�B5?BRB`�Bc B[�BC�BZ�Bb�BbBe�B^B[	BQ�B;JB:�B:�B4�BB�BB B�zB��B3BoB�aB��B��B�PB�B��B-B�<B�B�KB��B�PB��B��B}qB�$B��Be�BW�Be`BabBYKBJ�B0�B�BB�BB
�B iB
�[B
�|B
�EB
�RB
�TB
�)B
ZB
0�B
>�B
:�B
B�B
E9B
<PB
#�B	�GB	��B	�oB	�oB	�lB	��B	zxB	i�B	v�B	dZB	_VB	_B	d�B	`�B	KDB	U�B	B�B	,�B	/iB	!-B	�B	pB		RB	4B	4B	�B	B�LB�B�B�B޸B�BB��B�B�+B�_B��B�B��B�2B��B�HB��B��B��B��Bs�Bi�By$Bn}B`Bi�BxlBuZBl�B\�B\�Bg8Ba�BQ BY�BHKBW�BXyBW�BDBY1B`BBY�Bf�Bh�BabB[WB`�BZkBT{B`�BO�BDBG�B`�Bb�Bc B]/BYeB^BWsBVmBUgB_�Bb�B_BU�BP�BK�BO\BN�B`�B`B_B^�B`�B_�B\]B`Bd�BbB\]B`�Bd�Bd&Bb4Bl�BkBh$BhXBm�BpBnBnBnBhsBq�Bq'Bo5Bl"Ba�Bb�Bd�Bf�BoBoOBq'BoOBq[Bm�Bn�BwfByrBs�Bv�B}qB|jBu�Bv�B|�Bu�B��B�B�B��B��B�7B�)B�(B�xB��B�jB�TB�zB��B�hB��B�B��B��B�CB��B��B��B�B��B�3B�1B�fBȀB�NBՁB��B�~B��B�B��B��B��B��B�B�B�LB�(B	'B	AB	aB	YB	YB	YB	tB	�B	�B	�B	�B	�B	�B	!�B	# B	!-B	&2B	1'B	2aB	3�B	8�B	=�B	>�B	:*B	?�B	J�B	J�B	J�B	IB	IlB	R:B	VmB	X�B	`vB	a�B	c�B	j�B	j�B	j�B	n�B	o�B	p�B	s�B	u�B	u�B	s�B	r�B	v�B	xB	xB	wLB	yXB	HB	�aB	�_B	�fB	��B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�@B	�$B	�XB	�"B	�WB	�WB	�eB	�}B	��B	��B	��B	��B	�lB	�qB	�UB	��B	�2B	��B	ðB	ðB	ǔB	��B	�B	�+B	�)B	�B	�B	�B	�B	�.B	�4B	�:B	� B	�HB	�B	�9B	�2B	�NB	�,B	�9B	�MB	�YB	�=B	�=B	�kB	�KB	�eB	�_B	�YB	�yB	�_B	�OB	�IB	�CB	�eB	ؓB	�]B	�]B	�kB	�dB	�~B	ބB	�bB	�hB	�B	�nB	�B	�B	�zB	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�8B	�*B	�B	�B	�6B	�*B	�*B	�<B	�"B	�B	�dB	�<B
 B
 B	�B	�VB	�VB	�(B
 OB
B
B
aB
-B
AB
UB
-B
B
%B
	7B
DB

=B
^B
xB

rB
	lB
xB
xB
~B
vB
hB
�B
hB
oB
gB
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
!�B
"�B
#B
#B
!�B
"�B
#�B
$B
"�B
!B
"B
#B
#:B
$�B
&B
%B
'B
%B
%B
'B
'8B
)B
+B
+B
*B
)DB
)*B
)*B
+6B
*0B
*0B
,"B
,=B
,=B
./B
/B
/B
./B
,=B
+QB
/B
/OB
./B
/5B
.IB
,=B
,=B
.IB
/OB
2GB
2GB
1AB
1AB
2GB
1AB
2GB
3hB
2GB
3hB
2aB
1[B
1[B
2|B
4TB
4�B
6FB
6`B
5ZB
5ZB
6`B
6`B
6`B
6`B
5�B
7fB
7fB
7fB
7fB
7fB
7fB
7fB
7�B
7fB
8lB
9rB
8�B
9rB
9rB
9rB
9rB
9rB
9rB
9rB
9�B
:xB
:�B
;B
;B
<jB
;�B
;B
;B
:�B
:�B
:�B
:�B
<�B
<�B
=�B
=�B
=�B
=�B
<�B
=�B
?}B
>�B
>�B
>�B
?}B
>�B
=�B
>�B
?�B
?�B
A�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
?�B
@�B
A�B
B�B
A�B
@�B
?�B
A�B
A�B
A�B
A�B
A�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
H�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
J�B
K�B
K�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
J�B
K�B
I�B
J�B
K�B
L�B
L�B
M�B
MB
NB
O�B
PB
PB
O�B
O�B
O�B
O�B
O�B
P.B
Q B
RB
Q�B
Q�B
PB
OB
Q B
QB
QB
RB
RB
SB
SB
TB
TB
TB
T,B
T�B
T�B
U2B
TB
T�B
TB
UB
SB
S@B
UB
UB
T,B
W$B
W?B
XB
W?B
W$B
W$B
W?B
W$B
W$B
W$B
V9B
V9B
XEB
W?B
X+B
Y1B
Y1B
Y1B
Y1B
Y1B
XEB
W?B
W?B
YKB
Y1B
YeB
[=B
ZQB
\CB
\CB
\CB
\CB
[WB
\CB
\CB
]IB
^OB
^5B
]IB
^5B
]dB
[qB
\]B
\CB
[WB
\]B
^OB
^jB
^OB
_pB
_pB
`\B
abB
aHB
abB
`vB
`\B
bhB
bNB
bhB
abB
a|B
abB
cTB
cnB
cnB
b�B
b�B
cnB
dZB
dtB
d�B
dtB
dtB
dtB
d�B
dtB
dtB
c�B
d�B
cnB
b�B
c�B
ezB
f�B
f�B
f�B
f�B
f�B
ezB
f�B
gmB
g�B
ezB
hsB
g�B
f�B
f�B
g�B
g�B
hsB
h�B
g�B
g�B
i�B
i�B
j�B
jB
jB
j�B
j�B
k�B
jB
j�B
i�B
h�B
j�B
k�B
k�B
j�B
k�B
k�B
j�B
i�B
j�B
k�B
k�B
k�B
m�B
m�B
l�B
l�B
m�B
l�B
n�B
o�B
o�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
p�B
r�B
r�B
s�B
r�B
r�B
q�B
q�B
q�B
s�B
r�B
s�B
s�B
t�B
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
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808100036052018081000360520180810003605201808100200242018081002002420180810020024201808110028132018081100281320180811002813  JA  ARFMdecpA19c                                                                20180806093526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180806003700  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180806003705  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180806003706  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180806003707  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180806003707  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180806003707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180806003707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180806003714  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180806003715                      G�O�G�O�G�O�                JA  ARUP                                                                        20180806010042                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180806153653  CV  JULD            G�O�G�O�Fû�                JM  ARCAJMQC2.0                                                                 20180809153605  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180809153605  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180809170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180810152813  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                