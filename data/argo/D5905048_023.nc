CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-01T00:35:21Z creation;2016-08-01T00:35:23Z conversion to V3.1;2019-12-19T08:30:25Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160801003521  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_023                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׿��� 1   @׿���-�@3�$�/�d����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��C��C��C��C��C	��C��C��C��C��C��C\C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw�)Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD(�D(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV��DV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk��Dk�qDl}qDl�qDm��Dm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�{�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�D���D��D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�A�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D���D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D���D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aޕ�AޓuAޏ\AދDAމ7Aއ+AޑhAޅAރAޅAޅAރAށAށA�~�A�|�A�z�A�x�A�p�A��A��mA�v�A��/A���AѓuA��;A�ffA�A�A� �A���A̓A���A˸RA��A�dZAǶFA�oAƝ�A�+A�ZA���A�x�A��A�XA��uA���A���A�1'A��A���A��!A��7A�x�A�^5A�ZA�A���A�XA�S�A���A��A��A�(�A��A�/A��wA��\A���A��A�oA��TA�ȴA�~�A�ffA��A�n�A���A�|�A�33A��A��A�A�=qA��`A��A��A���A�bA���A�"�A���A��7A�?}A���A�G�A�&�A�ffA� �A~v�A|  AyVAu��Ar�`AqK�An�RAlȴAk;dAiG�Ag�#Ag�Ag�Af�Ae�Ad�Act�Ab  A`�`A`{A_hsA]�FA\ȴA\v�A[��AZ�\AY�;AYhsAW"�AT�AS%AQS�AP��AP�9AP�AM��AL�uAI`BAHr�AG�AG+AE��AD�`AC�AA�A@r�A>��A=�PA=A:�/A9l�A6�+A4ffA2ĜA1\)A0VA0 �A/�TA/��A.ZA,�9A+�A*��A)�-A(��A(5?A'�FA'`BA'/A&�9A&JA%��A$�\A#�
A"�yA!�-A ��A��A��AM�A��A�yAA5?A�`A��AA{A�yA �AƨAVA�hA��AbNAS�A-AI�A��A��A$�A�A�
A
I�A	t�A�A\)A��A;dAjA�mAA�A ��@�33@�Z@�dZ@��@�1'@��j@��@�1'@�K�@�@�7@�&�@���@�l�@��/@�Q�@��@�Q�@�|�@��@��;@��
@�
=@��@◍@�@�I�@�n�@ݩ�@���@ܬ@�"�@ج@�z�@��
@��@ղ-@�O�@��/@��;@�K�@�ȴ@��@���@�J@��T@��@�33@���@�x�@��@��;@���@ʇ+@��@���@�ff@��/@�r�@�1'@�1@ǶF@�l�@ǥ�@� �@��H@�1@�ƨ@î@+@�O�@�z�@�\)@�=q@�ȴ@�~�@���@�G�@�&�@���@���@���@��/@�Q�@��!@�v�@�M�@��@��u@��@�9X@�l�@��@���@��+@�~�@�ff@�E�@�M�@�v�@���@�ȴ@��w@�A�@��@�?}@��u@�o@���@�I�@��@���@��T@�7L@��@��\@�$�@�{@�^5@�v�@��@���@�/@��@��`@��D@�(�@�  @��`@�bN@��@���@�p�@��@�/@��@���@�A�@��F@��\@���@�v�@���@���@��@�@��F@���@�{@�@�-@�V@�@��@�Q�@��P@���@��R@�"�@�+@���@�^5@��@�C�@��@���@���@���@�G�@��-@��@�@�/@��D@�
=@�x�@�r�@���@�bN@�&�@�J@�b@��@�X@�/@��@�+@�9X@��
@�K�@�+@��!@�p�@�G�@��7@�X@�X@���@��u@��j@�Ĝ@��@���@��@�Z@�I�@� �@���@�|�@�l�@�S�@�;d@�@�~�@�-@��@�{@��@��@���@��h@�p�@�7L@�V@��`@���@��@���@�|�@�t�@�\)@�
=@�v�@�@��^@��h@�hs@�G�@�?}@��@�V@���@�j@�9X@��m@�dZ@�33@���@�v�@��@��@��^@��h@�?}@��@���@��@� �@��@��P@��@��H@��R@���@�M�@���@�p�@�7L@��@��j@�C�@�n�@�=q@�J@�-@�hs@��`@���@��@�9X@�\)@��y@��R@��+@���@�O�@�?}@�`B@�x�@�V@�j@�I�@��
@�ƨ@��@�C�@��H@�ȴ@���@�^5@��-@�`B@�G�@��@��`@�Ĝ@��@���@��u@�j@�(�@�;@�;@�;@�@�P@~�R@~5?@}�@}�-@}p�@}�h@}�@}V@|�@|�/@|��@|Z@{��@{��@{��@{ƨ@{��@{"�@zM�@yX@x�u@x�@xr�@w�P@w;d@vff@v$�@u�@u�h@u/@tj@s�@s33@r�H@r�\@r^5@q�#@q�7@q7L@pĜ@o�@o
=@nȴ@nff@m�T@mp�@mO�@l�@l�D@k�F@k"�@ko@j�@j�!@jJ@i��@ihs@iX@iG�@i7L@i&�@i%@h��@hQ�@g�@g|�@f�R@e��@e��@e/@eV@d��@d�j@d(�@cƨ@ct�@cdZ@c33@b�!@b~�@b-@bJ@a�^@a7L@`��@`Q�@_�w@_;d@^��@^5?@]�T@]@]��@]O�@\��@\��@\�D@\I�@\�@[��@[ƨ@[�@[o@Z�H@Zn�@ZM�@ZJ@Y�^@Y7L@X��@X�u@XQ�@X1'@X1'@W��@W|�@W�@V�@V��@V�+@V{@U�T@U�@T��@T��@T�@Tj@T(�@T1@S�m@S��@SS�@S33@R�@R-@Q�#@Q�^@Q7L@P�9@Pb@O�w@O�@N��@N@M�h@MO�@Mp�@MO�@M�@L��@LZ@L�@K�m@K��@KS�@K@J�!@J�\@JM�@J=q@JJ@I�@I�@I��@IG�@I�@H�@G��@G�w@G�@G��@G;d@F��@Fv�@E�@E�T@E@E�-@E��@EV@D�@C�F@Cƨ@CS�@C@B��@B�\@B^5@BM�@BM�@A�7@A&�@@�`@@�@?��@?\)@?
=@>�R@=��@=/@<��@<1@;�@;S�@;o@:��@:M�@9�@9hs@8�`@8Ĝ@8�9@8Q�@8  @7��@7��@7K�@6��@6ȴ@6��@6ff@6$�@5�@5��@5@5p�@5?}@4�/@4�@4j@41@3�
@3�F@3�@2�@2�!@2^5@1�@1��@1��@17L@0��@0��@0A�@/�@/�w@/|�@/K�@/+@.�@.��@.V@-@-?}@,��@,�/@,��@,�j@,��@+��@+dZ@+"�@*��@*n�@*-@)��@)��@)hs@)�@(�9@(r�@(bN@( �@(  @'�;@'��@'�@&�@&�R@&��@&��@&V@&5?@&{@%��@%�-@%��@%�h@%�@%p�@%`B@%O�@%?}@%�@$��@$�/@$�j@$Z@$9X@#��@#��@#C�@#@"��@"�!@"~�@"M�@!��@!�#@!�7@!&�@ ��@ r�@ bN@ Q�@�@�w@|�@K�@K�@��@v�@ff@E�@�@�h@p�@V@�@�/@��@�j@��@�D@Z@��@�@t�@dZ@S�@o@��@��@��@��@�^@�^@��@x�@&�@%@��@r�@Q�@b@�;@l�@�@��@��@�+@�+@�+@�+@ff@5?@{@�T@��@@�-@��@�h@O�@�@j@Z@Z@I�@(�@1@�m@ƨ@��@�@o@��@n�@n�@-@��@��@�#@�^@��@x�@7L@�`@��@�u@�@�@r�@bN@Q�@1'@b@�;@��@�@�P@K�@;d@��@�@��@v�@ff@E�@5?@$�@{@�T@��@�h@?}@/@`B@`B@O�@/@�@��@�/@Z@9X@Z@z�@Z@I�@�@�F@�@S�@33@
�@
��@
��@
��@
��@
�\@
^5@
=q@
J@
J@	�@	�^@	�^@	��@	�^@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aޕ�AޓuAޏ\AދDAމ7Aއ+AޑhAޅAރAޅAޅAރAށAށA�~�A�|�A�z�A�x�A�p�A��A��mA�v�A��/A���AѓuA��;A�ffA�A�A� �A���A̓A���A˸RA��A�dZAǶFA�oAƝ�A�+A�ZA���A�x�A��A�XA��uA���A���A�1'A��A���A��!A��7A�x�A�^5A�ZA�A���A�XA�S�A���A��A��A�(�A��A�/A��wA��\A���A��A�oA��TA�ȴA�~�A�ffA��A�n�A���A�|�A�33A��A��A�A�=qA��`A��A��A���A�bA���A�"�A���A��7A�?}A���A�G�A�&�A�ffA� �A~v�A|  AyVAu��Ar�`AqK�An�RAlȴAk;dAiG�Ag�#Ag�Ag�Af�Ae�Ad�Act�Ab  A`�`A`{A_hsA]�FA\ȴA\v�A[��AZ�\AY�;AYhsAW"�AT�AS%AQS�AP��AP�9AP�AM��AL�uAI`BAHr�AG�AG+AE��AD�`AC�AA�A@r�A>��A=�PA=A:�/A9l�A6�+A4ffA2ĜA1\)A0VA0 �A/�TA/��A.ZA,�9A+�A*��A)�-A(��A(5?A'�FA'`BA'/A&�9A&JA%��A$�\A#�
A"�yA!�-A ��A��A��AM�A��A�yAA5?A�`A��AA{A�yA �AƨAVA�hA��AbNAS�A-AI�A��A��A$�A�A�
A
I�A	t�A�A\)A��A;dAjA�mAA�A ��@�33@�Z@�dZ@��@�1'@��j@��@�1'@�K�@�@�7@�&�@���@�l�@��/@�Q�@��@�Q�@�|�@��@��;@��
@�
=@��@◍@�@�I�@�n�@ݩ�@���@ܬ@�"�@ج@�z�@��
@��@ղ-@�O�@��/@��;@�K�@�ȴ@��@���@�J@��T@��@�33@���@�x�@��@��;@���@ʇ+@��@���@�ff@��/@�r�@�1'@�1@ǶF@�l�@ǥ�@� �@��H@�1@�ƨ@î@+@�O�@�z�@�\)@�=q@�ȴ@�~�@���@�G�@�&�@���@���@���@��/@�Q�@��!@�v�@�M�@��@��u@��@�9X@�l�@��@���@��+@�~�@�ff@�E�@�M�@�v�@���@�ȴ@��w@�A�@��@�?}@��u@�o@���@�I�@��@���@��T@�7L@��@��\@�$�@�{@�^5@�v�@��@���@�/@��@��`@��D@�(�@�  @��`@�bN@��@���@�p�@��@�/@��@���@�A�@��F@��\@���@�v�@���@���@��@�@��F@���@�{@�@�-@�V@�@��@�Q�@��P@���@��R@�"�@�+@���@�^5@��@�C�@��@���@���@���@�G�@��-@��@�@�/@��D@�
=@�x�@�r�@���@�bN@�&�@�J@�b@��@�X@�/@��@�+@�9X@��
@�K�@�+@��!@�p�@�G�@��7@�X@�X@���@��u@��j@�Ĝ@��@���@��@�Z@�I�@� �@���@�|�@�l�@�S�@�;d@�@�~�@�-@��@�{@��@��@���@��h@�p�@�7L@�V@��`@���@��@���@�|�@�t�@�\)@�
=@�v�@�@��^@��h@�hs@�G�@�?}@��@�V@���@�j@�9X@��m@�dZ@�33@���@�v�@��@��@��^@��h@�?}@��@���@��@� �@��@��P@��@��H@��R@���@�M�@���@�p�@�7L@��@��j@�C�@�n�@�=q@�J@�-@�hs@��`@���@��@�9X@�\)@��y@��R@��+@���@�O�@�?}@�`B@�x�@�V@�j@�I�@��
@�ƨ@��@�C�@��H@�ȴ@���@�^5@��-@�`B@�G�@��@��`@�Ĝ@��@���@��u@�j@�(�@�;@�;@�;@�@�P@~�R@~5?@}�@}�-@}p�@}�h@}�@}V@|�@|�/@|��@|Z@{��@{��@{��@{ƨ@{��@{"�@zM�@yX@x�u@x�@xr�@w�P@w;d@vff@v$�@u�@u�h@u/@tj@s�@s33@r�H@r�\@r^5@q�#@q�7@q7L@pĜ@o�@o
=@nȴ@nff@m�T@mp�@mO�@l�@l�D@k�F@k"�@ko@j�@j�!@jJ@i��@ihs@iX@iG�@i7L@i&�@i%@h��@hQ�@g�@g|�@f�R@e��@e��@e/@eV@d��@d�j@d(�@cƨ@ct�@cdZ@c33@b�!@b~�@b-@bJ@a�^@a7L@`��@`Q�@_�w@_;d@^��@^5?@]�T@]@]��@]O�@\��@\��@\�D@\I�@\�@[��@[ƨ@[�@[o@Z�H@Zn�@ZM�@ZJ@Y�^@Y7L@X��@X�u@XQ�@X1'@X1'@W��@W|�@W�@V�@V��@V�+@V{@U�T@U�@T��@T��@T�@Tj@T(�@T1@S�m@S��@SS�@S33@R�@R-@Q�#@Q�^@Q7L@P�9@Pb@O�w@O�@N��@N@M�h@MO�@Mp�@MO�@M�@L��@LZ@L�@K�m@K��@KS�@K@J�!@J�\@JM�@J=q@JJ@I�@I�@I��@IG�@I�@H�@G��@G�w@G�@G��@G;d@F��@Fv�@E�@E�T@E@E�-@E��@EV@D�@C�F@Cƨ@CS�@C@B��@B�\@B^5@BM�@BM�@A�7@A&�@@�`@@�@?��@?\)@?
=@>�R@=��@=/@<��@<1@;�@;S�@;o@:��@:M�@9�@9hs@8�`@8Ĝ@8�9@8Q�@8  @7��@7��@7K�@6��@6ȴ@6��@6ff@6$�@5�@5��@5@5p�@5?}@4�/@4�@4j@41@3�
@3�F@3�@2�@2�!@2^5@1�@1��@1��@17L@0��@0��@0A�@/�@/�w@/|�@/K�@/+@.�@.��@.V@-@-?}@,��@,�/@,��@,�j@,��@+��@+dZ@+"�@*��@*n�@*-@)��@)��@)hs@)�@(�9@(r�@(bN@( �@(  @'�;@'��@'�@&�@&�R@&��@&��@&V@&5?@&{@%��@%�-@%��@%�h@%�@%p�@%`B@%O�@%?}@%�@$��@$�/@$�j@$Z@$9X@#��@#��@#C�@#@"��@"�!@"~�@"M�@!��@!�#@!�7@!&�@ ��@ r�@ bN@ Q�@�@�w@|�@K�@K�@��@v�@ff@E�@�@�h@p�@V@�@�/@��@�j@��@�D@Z@��@�@t�@dZ@S�@o@��@��@��@��@�^@�^@��@x�@&�@%@��@r�@Q�@b@�;@l�@�@��@��@�+@�+@�+@�+@ff@5?@{@�T@��@@�-@��@�h@O�@�@j@Z@Z@I�@(�@1@�m@ƨ@��@�@o@��@n�@n�@-@��@��@�#@�^@��@x�@7L@�`@��@�u@�@�@r�@bN@Q�@1'@b@�;@��@�@�P@K�@;d@��@�@��@v�@ff@E�@5?@$�@{@�T@��@�h@?}@/@`B@`B@O�@/@�@��@�/@Z@9X@Z@z�@Z@I�@�@�F@�@S�@33@
�@
��@
��@
��@
��@
�\@
^5@
=q@
J@
J@	�@	�^@	�^@	��@	�^@	��@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�BVBŢB��BBPBoBuB�B(�B0!B9XBA�BN�BgmBp�B{�B�%B�PB��B��B�XB�jB�9B��B��B�7Bx�BiyB2-B�BbBPB	7BB��B�B�B�B�TB�mB�B�B�yB�B�B�B��B#�BI�BI�B
=B�5B�)B�TB�yB�sB�#B�B��B��B��B�bB�=Bv�BaHB?}B�B+B
��B
�yB
�B
��B
ĜB
�XB
�'B
�B
��B
�bB
� B
n�B
S�B
C�B
5?B
+B
�B

=B
  B	�B	�mB	�`B	�TB	�HB	�)B	�B	��B	ȴB	��B	�dB	�LB	�B	��B	��B	��B	��B	�oB	�JB	�B	n�B	e`B	\)B	VB	T�B	T�B	F�B	A�B	7LB	1'B	.B	(�B	#�B	�B	�B	hB	
=B	B��B��B�B�mB�;B�B��B��BƨBŢBĜBÖB��B�^B�LB�?B�-B�B�B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�JB�=B�1B�B�B�B~�B|�Bz�By�Bx�Bu�Bt�Bs�Bt�Bu�B�B�VB��B��B��B��B�=B�B}�BjBe`BgmBo�Br�Br�B{�B�B~�Bu�Bs�Bt�Bv�B~�B�\B��B�DB�bB��B��B�uB�VB�DB�VB�oB�oB�{B��B��B�B�B�3B�9B�9B�'B�!B�!B�?B�RB�wB�}BBŢBȴB��B��B��B��B�
B�
B��B�B�B�B�B�B��B��B�#B�B�B�B�ZB�mB�B�B�B��B��B��B��B	B	JB	
=B	B	B	%B	
=B	+B	
=B	PB	DB	uB	�B	�B	�B	�B	�B	�B	)�B	1'B	2-B	1'B	7LB	:^B	=qB	=qB	@�B	I�B	J�B	J�B	J�B	K�B	L�B	M�B	N�B	P�B	S�B	VB	]/B	gmB	l�B	s�B	w�B	x�B	r�B	o�B	jB	l�B	p�B	v�B	w�B	q�B	o�B	n�B	o�B	p�B	z�B	x�B	~�B	�B	|�B	t�B	s�B	s�B	u�B	~�B	�B	~�B	{�B	|�B	}�B	�B	�B	�JB	�=B	�JB	�JB	�hB	�hB	�bB	�oB	��B	��B	�B	�B	�B	�B	�-B	�9B	�?B	�FB	�?B	�3B	�!B	�-B	�FB	�FB	�FB	�FB	�^B	�wB	�dB	�^B	��B	��B	��B	�#B	�5B	�;B	�;B	�5B	�B	��B	��B	��B	��B	�B	�;B	�B	��B	��B	��B	�B	�5B	�fB	�fB	�fB	�mB	�mB	�ZB	�ZB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
JB
JB
JB
PB
PB
VB
VB
\B
bB
hB
oB
oB
hB
VB
DB
JB
JB
PB
PB
DB

=B

=B
	7B
1B
	7B
	7B
PB
PB
PB
VB
bB
hB
hB
bB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
2-B
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
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
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
;dB
;dB
;dB
;dB
;dB
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
A�B
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
H�B
I�B
I�B
I�B
H�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
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
cTB
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
gmB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
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
n�B
n�B
n�B
o�B
o�B
o�B
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
p�B
p�B
p�B
p�B
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
w�B
w�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
z�B
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
{�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~B!B*�B^�BˬB��BaB�B�B�B�B)�B1'B;0BD�BSuBh�BrB}<B�EB��B��B�XB�jB�B��B��B��B��B}qBp!B5�B�BB�BJBEB�6B�ZB�B�[B�B�B�WB�B�6B��B��B��B��B%,BN<BOvB"B�!B�B��B�qB�"B�jB�1B�bBżB�&B��B�jBz^Bf2BC�B#�B	�B
��B
��B
��B
�aB
żB
��B
�B
�AB
��B
��B
�GB
rGB
W�B
F�B
7�B
-�B
�B
JB
'B	�B	�$B	��B	��B	�B	ݲB	��B	өB	�	B	��B	��B	�	B	�B	��B	��B	��B	��B	��B	�(B	��B	p�B	g8B	\�B	V�B	V�B	W�B	H�B	D�B	8�B	2B	/OB	*�B	%FB	 �B	�B	�B	dB	�B�VB�xB�B�B��B�BЗB��B�B�?B�mB�mB�uB��B��B��B�hB��B��B��B��B��B��B��B�2B�B�-B�VB�)B��B��B�[B�oB��B��B�~B��B��B��B�3B��B~B{�B{�By�Bv�Bu�Bu%Bu�BvB��B�VB��B��B��B��B��B��B��Bl�BfLBh�BpUBs3Bs�B|�B�{B��Bv�Bu%BuBv�B}B�}B�QB�dB� B�kB��B�2B��B�B��B��B�B��B�SB��B��B�cB��B�B�ZB�GB��B��B��B��B��B��B�-B�YBɆB�"B�\BѷB�gB�sB�sB�2B�BٚB��B�7B��B�gBՁB��BٴB�9B�1B��B�B�B��B��B��B�B�B��B	;B	PB	�B	aB	�B	B	)B	�B	B	�B	DB	�B	B	�B	�B	�B	�B	�B	*B	1�B	33B	1vB	7�B	:�B	>BB	=�B	@�B	J#B	K)B	J�B	J�B	K�B	L�B	M�B	N�B	P�B	S�B	U�B	\�B	gB	l=B	s�B	xlB	y�B	s�B	pUB	jB	l=B	poB	wfB	x�B	raB	o�B	n�B	o�B	qB	{B	x�B	�B	�uB	~]B	u%B	s�B	s�B	u�B	}B	��B	�B	|6B	}"B	}�B	�'B	�3B	�6B	��B	��B	�JB	��B	��B	�}B	� B	�
B	��B	��B	�]B	�/B	�B	�GB	��B	��B	��B	��B	��B	�!B	�B	�FB	�zB	��B	�B	��B	�B	�dB	��B	�iB	ѷB	��B	�#B	�jB	߾B	��B	�;B	�#B	�{B	��B	�B	өB	�B	�\B	��B	��B	��B	�PB	�MB	��B	�B	��B	�B	��B	�$B	�B	�tB	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	��B	��B	��B	��B	�B	�B	�"B	�"B	�BB	�]B	�wB
 4B
 B
 B
 4B
 OB
oB
uB
GB
GB
GB
GB
GB
-B
-B
GB
gB
MB
gB
mB
YB
tB
zB
zB
KB
fB
	lB
	lB
	lB

�B

rB

�B
�B
~B
�B
�B
�B
pB
�B
�B
�B
�B
�B
B
TB
�B
xB
dB
dB
�B
�B
�B

�B

�B
	�B
�B
	lB
	�B
�B
�B
jB
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"B
#:B
$&B
%,B
$�B
%B
%,B
%B
&2B
'B
%�B
'B
'B
'RB
(XB
(
B
($B
'B
(
B
($B
($B
)DB
)*B
)_B
)DB
)B
)*B
*0B
*0B
+B
+6B
+6B
,WB
-CB
-)B
-)B
-)B
-CB
-CB
./B
.B
.B
./B
./B
./B
.IB
.cB
/OB
/OB
/iB
0oB
/OB
/iB
0UB
1AB
1[B
1vB
2GB
2aB
2GB
2aB
2|B
3MB
2aB
3MB
3hB
3hB
3hB
3hB
4nB
4nB
5�B
5tB
5ZB
6`B
6`B
6`B
7�B
7fB
7fB
7fB
7fB
7fB
7fB
8lB
8�B
8lB
8�B
9�B
9rB
9�B
9�B
9rB
:xB
:xB
:xB
:xB
:�B
:xB
;B
;B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
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
A�B
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
IB
I�B
I�B
I�B
H�B
I�B
I�B
KB
J�B
J�B
J�B
J�B
J�B
KB
J�B
J�B
MB
L�B
MB
MB
L�B
L�B
L�B
MB
M�B
NB
OB
O(B
PB
PB
PB
P.B
PB
PB
PHB
P.B
Q B
Q B
QB
QB
R B
R:B
S&B
SB
SB
S&B
SB
TB
TB
TB
T,B
TB
TB
TB
UB
U2B
U2B
UB
UB
UB
UB
VB
VB
V9B
VB
VB
VB
V9B
W$B
W$B
WYB
X+B
XEB
Y1B
YKB
ZQB
ZQB
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
ZQB
[qB
[=B
[=B
[#B
[=B
[=B
[qB
[WB
\CB
\CB
\CB
\]B
\CB
\]B
\]B
\]B
]IB
]IB
]dB
]IB
^jB
^OB
^OB
^jB
_VB
_VB
_VB
_VB
_VB
`\B
abB
abB
bhB
bNB
bhB
bNB
bhB
cTB
cTB
cTB
cnB
c�B
cnB
cnB
dtB
dtB
d�B
dtB
d�B
ezB
e�B
e�B
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
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
g�B
g�B
g�B
h�B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
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
l�B
l�B
l�B
l�B
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
n�B
n�B
n�B
o�B
o�B
o�B
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
p�B
p�B
p�B
p�B
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
w�B
w�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
y�B
y�B
z�B
z�B
z�B
{B
{B
z�B
|B
|B
{�B
{�B
{�B
{�B
|B
|B
z�B
z�B
z�B
z�B
z�B
z�B
|B
{�B
|B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608050040192016080500401920160805004019201806221300102018062213001020180622130010201804050658592018040506585920180405065859  JA  ARFMdecpA19c                                                                20160801093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160801003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160801003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160801003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160801003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160801003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160801003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160801003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160801003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160801003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20160801012041                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160801153434  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160801153434  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160801153434  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20160801153434  CV  LONGITUDE       G�O�G�O��$�}                JM  ARCAJMQC2.0                                                                 20160804154019  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160804154019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215859  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040010  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                