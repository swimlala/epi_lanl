CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-31T00:36:20Z creation;2018-12-31T00:36:26Z conversion to V3.1;2019-12-19T07:24:30Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20181231003620  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              <A   JA  I2_0576_316                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؜4� 1   @؜5 @9\(��dB�Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��RA\)A?\)A_\)A\)A�z�A��A��A��AϮA߮A�A��HB�
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
B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D��D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�A�Dׁ�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD��D�>�D���D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�E1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��hA��hA��\A��\A��hA��\A��PA��PA��\A��\A��\A��hA��hA��PA��DA��DA��PA��PA��PA��PA��PA��PA��PA��\A��PA��PA��PA��PA��PA��PA��PA��PA��PA��PA��DA��7A��A�t�A�hsA�I�A�"�A�x�A���A��A��A��9A�VA��A� �A���A�G�A��A��A���A�9XA���A�7LA�1A��HA���A�+A�S�A��FA�33A��;A��A�S�A���A�\)A�|�A���A�z�A�?}A��TA�1A��wA���A���A���A���A�hsA��\A�7LA�\)A��HA��A�oA��A�r�A���A�1'A�+A�\)A���A��\A�I�A�\)A�&�A~��Az��Ay+Ax�Av�DAtA�Ar�HAq�mAp�\Ap$�Ao?}AnȴAm��Am�Al��Akl�Ajr�Ag�7Ae�Ae�-Ae`BAd�+Ad=qAc�;Ac�Ac\)Ac�Ab��AbȴAb~�Ab{A`�HA_;dAZ��AX�AX�9AX=qAW�mAW��AWp�AV��AT�+AS`BAR��ARZAQ��AQC�AP�APz�APffAO��ALȴAL$�AKAJ�9AI|�AHI�AE�AC�FAB��ABbNAA�A@�A?A>~�A>1A=K�A<�A;`BA:bNA8-A6�RA5�A2�uA1S�A0^5A/K�A.��A.�HA.n�A-l�A,�A,�+A,=qA+�A+|�A*��A)�A)x�A(�\A(1A';dA&bA$�A#�A#�-A#|�A#�A!ƨA��Al�A��A-A�mA�^A;dAA��A�AI�A$�A�#A�Az�AA`BA�AM�A1A�7A��A�HA��A�RAv�A�wA�A�Al�A�A
��A
��A	��A��AA�A��AC�AoA�7A��A �R@�\)@���@� �@���@�|�@�K�@�C�@�@��@���@�-@��@��F@��@@�=q@���@�V@�  @�E�@�%@�(�@�ȴ@�@�9@��@��/@�Z@�  @߮@߮@�+@�x�@�ff@�O�@���@�1'@ץ�@�ȴ@�n�@��@��@�p�@���@ύP@��@�
=@��@���@�33@�M�@ɩ�@�r�@�@ŉ7@ċD@�1@�ƨ@ÍP@�t�@�33@�E�@�x�@���@��@��@�~�@�J@���@�"�@��-@�G�@���@�r�@�  @��F@�+@��@���@�-@�X@�I�@��@�dZ@���@��@�@��@�&�@�bN@��@��
@��w@�l�@�33@�=q@���@�(�@�ƨ@�+@��@�ȴ@��H@�S�@��y@�E�@���@���@���@�hs@�&�@��`@�bN@��;@���@�@��@�ƨ@��y@�$�@��@�`B@�O�@�V@�1@��@���@�l�@�S�@�K�@�\)@�;d@��@��R@�v�@�V@�@�V@�v�@�E�@��@�O�@� �@�K�@��@�n�@��T@���@���@���@�`B@���@��D@�9X@��
@���@��@��+@�@�G�@�7L@�&�@��@�%@��/@���@�z�@�(�@��w@���@�|�@�C�@�
=@���@�^5@�{@��@���@���@���@�x�@��/@�Z@�A�@�  @��m@��
@���@���@��w@���@���@�|�@�t�@�dZ@�K�@��R@�{@�@�G�@���@��@��@�ƨ@�+@��@�o@�
=@���@�n�@���@��@�Ĝ@�r�@�1@l�@~�@~ff@~5?@~{@}/@|��@|I�@|�@{��@{�
@{��@{t�@{S�@{C�@{"�@z�@z�\@y��@y�7@yG�@y%@x�u@x �@w�@w�;@w�w@w�P@wl�@w\)@wK�@w
=@v��@vE�@u�T@u�@t�@t�D@tZ@t(�@s��@s�F@sS�@so@r��@r~�@q�^@qG�@q&�@p��@pĜ@pA�@o�@o�@o��@ol�@oK�@o+@n�R@n$�@n@m@l��@k��@j�@i�@i�7@ihs@ihs@ihs@iG�@h�`@hĜ@h��@h��@hr�@h �@g�w@g�@g��@g�P@g\)@g+@fȴ@f�+@fff@fE�@fE�@fE�@fE�@f{@e��@e��@e�h@e`B@e?}@e/@d�@d��@d�@dj@d�@b^5@a%@`��@`r�@_�w@^ȴ@^$�@]�T@]�T@]O�@\�j@\�D@\�D@\j@\9X@[ƨ@Z��@Z^5@Y��@YG�@Y%@X�@W�@WK�@W;d@W�@W�@V�@U@UV@T�@T�j@TI�@T9X@T�@T�@T�@S�m@S"�@R�@R�H@R��@R��@Rn�@RM�@R�@Qx�@P�`@PA�@O�;@O�@O�@O�P@O�P@Ol�@OK�@N�@Nff@NE�@M�T@Mp�@L�@L�j@Lj@L(�@K��@K33@K"�@J�H@J~�@J^5@J=q@I�#@IG�@I%@HĜ@H�@H1'@H  @G��@G�w@G;d@Fȴ@F��@F��@F��@F5?@E��@E�@E?}@E/@E�@E�@D��@D��@D��@D�@D�@DZ@Cƨ@CC�@Co@B�@B��@B��@B�\@B~�@B~�@B~�@B~�@B~�@Bn�@Bn�@Bn�@BM�@A��@A��@@��@@r�@@A�@@b@?�@?�;@?\)@?;d@?
=@>�@>�@>��@>��@>�+@>ff@>$�@>@=p�@<j@;ƨ@;C�@:�@:�!@:�\@:�\@:�\@:�!@:��@:-@9��@9%@8��@8Ĝ@8r�@8Q�@8A�@8  @7�@7l�@6�y@6ȴ@6��@6��@6�+@6�+@6�+@6v�@6$�@5@5`B@4��@4z�@3�
@333@2��@2��@2�\@2~�@2^5@2-@1�#@1X@1%@0bN@/�;@/|�@/;d@/
=@/
=@.�y@.�y@.ff@-�T@-`B@,�/@,��@,Z@,(�@+t�@+o@+@*��@*^5@)��@)��@(Ĝ@(bN@(Q�@(1'@'��@'+@&��@&�R@&��@&��@&��@&�+@&V@&5?@&5?@&5?@&5?@&5?@&$�@&{@%�@%��@%�h@%?}@$�/@$I�@$1@#ƨ@#�@#S�@#33@#33@#33@#"�@#o@#o@#@"�H@"�H@"��@"�\@"=q@"J@!�^@!�7@!&�@ ��@ ��@ �9@ �u@ r�@ b@��@�@�P@\)@;d@K�@+@�@�R@��@v�@E�@$�@�@`B@�/@�j@��@j@1@�m@ƨ@��@t�@dZ@C�@��@n�@�@�^@&�@�`@�9@bN@ �@  @b@b@b@  @�;@�P@\)@K�@�@ȴ@��@��@�+@�+@ff@V@$�@�-@O�@O�@?}@?}@?}@V@�D@(�@��@�
@ƨ@dZ@33@o@��@��@�\@�\@~�@=q@J@�@�@�@��@��@��@�^@��@hs@7L@�@�`@�@ �@�;@�@\)@\)@;d@+@�@
=@��@ȴ@ȴ@�+@V@E�@E�@5?@$�@�T@p�@`B@O�@?}@?}@?}@/@�@�@�@��@�@I�@9X@(�@�@1@��@ƨ@��@S�@
�H@
�!@
�!@
�\@
n�@	��@	�7@	�7@	%@��@bN@ �@�w@�@�P@;d@�y@�@�R@�+@E�@{@�T@@��@�h@`B@`B@O�@?}@V@�@�j@z�@�m@�
@ƨ@�F@�@S�@o@�H@��@�!@��@��@��@�\@^5@-@��@X@%@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��hA��hA��\A��\A��hA��\A��PA��PA��\A��\A��\A��hA��hA��PA��DA��DA��PA��PA��PA��PA��PA��PA��PA��\A��PA��PA��PA��PA��PA��PA��PA��PA��PA��PA��DA��7A��A�t�A�hsA�I�A�"�A�x�A���A��A��A��9A�VA��A� �A���A�G�A��A��A���A�9XA���A�7LA�1A��HA���A�+A�S�A��FA�33A��;A��A�S�A���A�\)A�|�A���A�z�A�?}A��TA�1A��wA���A���A���A���A�hsA��\A�7LA�\)A��HA��A�oA��A�r�A���A�1'A�+A�\)A���A��\A�I�A�\)A�&�A~��Az��Ay+Ax�Av�DAtA�Ar�HAq�mAp�\Ap$�Ao?}AnȴAm��Am�Al��Akl�Ajr�Ag�7Ae�Ae�-Ae`BAd�+Ad=qAc�;Ac�Ac\)Ac�Ab��AbȴAb~�Ab{A`�HA_;dAZ��AX�AX�9AX=qAW�mAW��AWp�AV��AT�+AS`BAR��ARZAQ��AQC�AP�APz�APffAO��ALȴAL$�AKAJ�9AI|�AHI�AE�AC�FAB��ABbNAA�A@�A?A>~�A>1A=K�A<�A;`BA:bNA8-A6�RA5�A2�uA1S�A0^5A/K�A.��A.�HA.n�A-l�A,�A,�+A,=qA+�A+|�A*��A)�A)x�A(�\A(1A';dA&bA$�A#�A#�-A#|�A#�A!ƨA��Al�A��A-A�mA�^A;dAA��A�AI�A$�A�#A�Az�AA`BA�AM�A1A�7A��A�HA��A�RAv�A�wA�A�Al�A�A
��A
��A	��A��AA�A��AC�AoA�7A��A �R@�\)@���@� �@���@�|�@�K�@�C�@�@��@���@�-@��@��F@��@@�=q@���@�V@�  @�E�@�%@�(�@�ȴ@�@�9@��@��/@�Z@�  @߮@߮@�+@�x�@�ff@�O�@���@�1'@ץ�@�ȴ@�n�@��@��@�p�@���@ύP@��@�
=@��@���@�33@�M�@ɩ�@�r�@�@ŉ7@ċD@�1@�ƨ@ÍP@�t�@�33@�E�@�x�@���@��@��@�~�@�J@���@�"�@��-@�G�@���@�r�@�  @��F@�+@��@���@�-@�X@�I�@��@�dZ@���@��@�@��@�&�@�bN@��@��
@��w@�l�@�33@�=q@���@�(�@�ƨ@�+@��@�ȴ@��H@�S�@��y@�E�@���@���@���@�hs@�&�@��`@�bN@��;@���@�@��@�ƨ@��y@�$�@��@�`B@�O�@�V@�1@��@���@�l�@�S�@�K�@�\)@�;d@��@��R@�v�@�V@�@�V@�v�@�E�@��@�O�@� �@�K�@��@�n�@��T@���@���@���@�`B@���@��D@�9X@��
@���@��@��+@�@�G�@�7L@�&�@��@�%@��/@���@�z�@�(�@��w@���@�|�@�C�@�
=@���@�^5@�{@��@���@���@���@�x�@��/@�Z@�A�@�  @��m@��
@���@���@��w@���@���@�|�@�t�@�dZ@�K�@��R@�{@�@�G�@���@��@��@�ƨ@�+@��@�o@�
=@���@�n�@���@��@�Ĝ@�r�@�1@l�@~�@~ff@~5?@~{@}/@|��@|I�@|�@{��@{�
@{��@{t�@{S�@{C�@{"�@z�@z�\@y��@y�7@yG�@y%@x�u@x �@w�@w�;@w�w@w�P@wl�@w\)@wK�@w
=@v��@vE�@u�T@u�@t�@t�D@tZ@t(�@s��@s�F@sS�@so@r��@r~�@q�^@qG�@q&�@p��@pĜ@pA�@o�@o�@o��@ol�@oK�@o+@n�R@n$�@n@m@l��@k��@j�@i�@i�7@ihs@ihs@ihs@iG�@h�`@hĜ@h��@h��@hr�@h �@g�w@g�@g��@g�P@g\)@g+@fȴ@f�+@fff@fE�@fE�@fE�@fE�@f{@e��@e��@e�h@e`B@e?}@e/@d�@d��@d�@dj@d�@b^5@a%@`��@`r�@_�w@^ȴ@^$�@]�T@]�T@]O�@\�j@\�D@\�D@\j@\9X@[ƨ@Z��@Z^5@Y��@YG�@Y%@X�@W�@WK�@W;d@W�@W�@V�@U@UV@T�@T�j@TI�@T9X@T�@T�@T�@S�m@S"�@R�@R�H@R��@R��@Rn�@RM�@R�@Qx�@P�`@PA�@O�;@O�@O�@O�P@O�P@Ol�@OK�@N�@Nff@NE�@M�T@Mp�@L�@L�j@Lj@L(�@K��@K33@K"�@J�H@J~�@J^5@J=q@I�#@IG�@I%@HĜ@H�@H1'@H  @G��@G�w@G;d@Fȴ@F��@F��@F��@F5?@E��@E�@E?}@E/@E�@E�@D��@D��@D��@D�@D�@DZ@Cƨ@CC�@Co@B�@B��@B��@B�\@B~�@B~�@B~�@B~�@B~�@Bn�@Bn�@Bn�@BM�@A��@A��@@��@@r�@@A�@@b@?�@?�;@?\)@?;d@?
=@>�@>�@>��@>��@>�+@>ff@>$�@>@=p�@<j@;ƨ@;C�@:�@:�!@:�\@:�\@:�\@:�!@:��@:-@9��@9%@8��@8Ĝ@8r�@8Q�@8A�@8  @7�@7l�@6�y@6ȴ@6��@6��@6�+@6�+@6�+@6v�@6$�@5@5`B@4��@4z�@3�
@333@2��@2��@2�\@2~�@2^5@2-@1�#@1X@1%@0bN@/�;@/|�@/;d@/
=@/
=@.�y@.�y@.ff@-�T@-`B@,�/@,��@,Z@,(�@+t�@+o@+@*��@*^5@)��@)��@(Ĝ@(bN@(Q�@(1'@'��@'+@&��@&�R@&��@&��@&��@&�+@&V@&5?@&5?@&5?@&5?@&5?@&$�@&{@%�@%��@%�h@%?}@$�/@$I�@$1@#ƨ@#�@#S�@#33@#33@#33@#"�@#o@#o@#@"�H@"�H@"��@"�\@"=q@"J@!�^@!�7@!&�@ ��@ ��@ �9@ �u@ r�@ b@��@�@�P@\)@;d@K�@+@�@�R@��@v�@E�@$�@�@`B@�/@�j@��@j@1@�m@ƨ@��@t�@dZ@C�@��@n�@�@�^@&�@�`@�9@bN@ �@  @b@b@b@  @�;@�P@\)@K�@�@ȴ@��@��@�+@�+@ff@V@$�@�-@O�@O�@?}@?}@?}@V@�D@(�@��@�
@ƨ@dZ@33@o@��@��@�\@�\@~�@=q@J@�@�@�@��@��@��@�^@��@hs@7L@�@�`@�@ �@�;@�@\)@\)@;d@+@�@
=@��@ȴ@ȴ@�+@V@E�@E�@5?@$�@�T@p�@`B@O�@?}@?}@?}@/@�@�@�@��@�@I�@9X@(�@�@1@��@ƨ@��@S�@
�H@
�!@
�!@
�\@
n�@	��@	�7@	�7@	%@��@bN@ �@�w@�@�P@;d@�y@�@�R@�+@E�@{@�T@@��@�h@`B@`B@O�@?}@V@�@�j@z�@�m@�
@ƨ@�F@�@S�@o@�H@��@�!@��@��@��@�\@^5@-@��@X@%@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�bB�hB�hB�hB�bB�bB�bB�bB�\B�bB�bB�\B�\B�\B�\B�\B�\B�\B�\B�VB�\B�\B�\B�\B�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�VB�PB�DB�7B�Bz�Be`B�BBYBk�BiyBgmBO�BP�B\)BC�B7LB\)BR�B9XBL�BQ�BC�B�B�B��B�)B�B�B�LB��BR�Bk�B�B~�B~�B]/BjBT�B5?B;dB?}B"�BB
��B
ÖB
��B
��B
��B
��B
ɺB
ƨB
�dB
��B
��B
�B
�B
�B
�B
t�B
VB
?}B
1'B
VB
{B
�B
JB	�B	��B	��B	�B	��B	�B	�B	�mB	�BB	��B	ÖB	�dB	��B	��B	ĜB	��B	�FB	�}B	�jB	�dB	�dB	�RB	�LB	�'B	��B	��B	�B	ffB	:^B	ZB	~�B	x�B	u�B	s�B	iyB	XB	B�B	F�B	R�B	Q�B	O�B	G�B	F�B	G�B	@�B	/B	B	"�B	#�B	uB��B��B��B�B�B�B�B�HB��B�HB�)B��B��B�dB�3B��B��B��B� B�bB��B�hB��B��B��B�=B�oB�oB�oB�JB�JB�Bn�B�1Bq�Bs�Bk�BffBaHBjBw�Br�Be`BI�B,BI�BYBbNBaHB`BBS�BB�BF�BN�BE�B$�B�B8RBF�BC�B=qB@�B@�BB�BA�B>wBH�BG�BC�B:^B.B+B(�B6FB8RB6FB2-B �BuB	7B%�B.B'�BoB��B�B�B�B"�B2-B2-B33B49B2-B1'B-B&�B�BVB�BB�B)�B&�B"�B�B#�B'�B$�B$�B(�B!�B�B5?B8RB7LB6FB,B�BoB/B6FB6FB5?B1'B33B'�B"�B.B7LB7LB=qB@�B:^B)�B)�B7LB:^B49B49B5?BC�BI�BM�BL�BK�BF�B>wB=qB9XB-B&�BT�BP�BH�BK�BW
BgmBjBhsBk�Bn�Bm�Bp�Bp�Bn�Bl�Bl�Bw�Bx�By�B�B�\B�VB�oB�oB��B��B��B��B��B��B�hB��B��B��B��B�!B�3B�FB�'B�!B�FB�LB�LB�FB�FB�FB�3B�-B�B�-B�XB�?B�jBÖBȴB��B��B��BɺB��B�B�B�B�#B�B�B�B��B�B�
B��B��B�wB�BB�/B�B��B�B�NB�BB�ZB�B�B�B�B�B�B�B�B��B��B��B��B��B		7B	
=B	
=B	
=B	
=B		7B	DB	DB	JB	uB	uB	{B	�B	�B	�B	�B	 �B	!�B	"�B	!�B	!�B	 �B	&�B	1'B	1'B	49B	6FB	7LB	7LB	6FB	7LB	8RB	8RB	8RB	8RB	7LB	49B	7LB	?}B	A�B	D�B	K�B	K�B	O�B	P�B	]/B	]/B	]/B	\)B	`BB	_;B	^5B	hsB	n�B	p�B	v�B	x�B	{�B	~�B	}�B	|�B	� B	�B	�%B	�1B	�1B	�1B	�=B	�DB	�DB	�DB	�DB	�DB	�JB	�\B	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�!B	�?B	�9B	�9B	�9B	�3B	�-B	�-B	�?B	�3B	�!B	�-B	�3B	�?B	�XB	�qB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	ĜB	ǮB	ǮB	ǮB	ƨB	ƨB	ŢB	ƨB	ȴB	ɺB	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ĜB	��B	��B	�
B	��B	��B	�B	�)B	�5B	�)B	�/B	�NB	�TB	�NB	�HB	�5B	�/B	�HB	�TB	�`B	�mB	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B	��B	��B
B
  B
B
B
B
B
B
B
+B
	7B
1B
+B
	7B
	7B
+B
1B

=B
DB
DB
DB
PB
PB
\B
PB
VB
hB
oB
oB
hB
bB
uB
uB
{B
{B
{B
uB
uB
�B
uB
oB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
"�B
!�B
!�B
�B
�B
 �B
"�B
$�B
$�B
&�B
&�B
&�B
&�B
&�B
#�B
#�B
#�B
'�B
'�B
'�B
(�B
)�B
'�B
(�B
(�B
(�B
,B
,B
-B
-B
-B
-B
,B
+B
)�B
+B
+B
+B
+B
,B
/B
2-B
2-B
2-B
2-B
1'B
1'B
0!B
1'B
1'B
2-B
49B
6FB
8RB
9XB
8RB
7LB
5?B
33B
5?B
6FB
8RB
9XB
9XB
7LB
9XB
<jB
;dB
:^B
:^B
;dB
:^B
=qB
@�B
@�B
>wB
>wB
B�B
B�B
D�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
G�B
H�B
I�B
I�B
J�B
K�B
K�B
J�B
K�B
K�B
K�B
J�B
J�B
I�B
H�B
H�B
I�B
I�B
J�B
J�B
L�B
L�B
L�B
L�B
L�B
K�B
M�B
N�B
N�B
M�B
N�B
O�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
L�B
N�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
S�B
T�B
S�B
Q�B
R�B
R�B
T�B
S�B
W
B
XB
XB
YB
ZB
\)B
[#B
[#B
[#B
ZB
YB
ZB
[#B
[#B
ZB
\)B
]/B
]/B
]/B
\)B
\)B
\)B
[#B
[#B
_;B
_;B
^5B
^5B
]/B
\)B
]/B
_;B
_;B
_;B
^5B
_;B
`BB
`BB
`BB
bNB
bNB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
aHB
aHB
bNB
bNB
`BB
bNB
bNB
cTB
dZB
ffB
e`B
ffB
ffB
ffB
ffB
e`B
ffB
e`B
ffB
gmB
gmB
gmB
ffB
e`B
e`B
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
gmB
gmB
jB
jB
jB
jB
iyB
hsB
iyB
hsB
iyB
jB
l�B
k�B
jB
iyB
jB
l�B
jB
jB
m�B
m�B
m�B
o�B
n�B
m�B
n�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
p�B
s�B
s�B
t�B
r�B
r�B
r�B
s�B
u�B
u�B
u�B
v�B
v�B
u�B
t�B
t�B
t�B
t�B
v�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�bB�hB�hB�hB�bB�bB�bB�bB�\B�bB�bB�\B�\B�\B�\B�\B�\B�\B�\B�VB�\B�\B�\B�vB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�VB�jB�xB�lB��B|Bg�B&�B�BZ�BmBkBiBR�BS�B]�BG_B:*B]/BTaB<�BM�BR�BD�B�B�BЗB�pB�B�#B�^B�'BZ7Bo B��B�OB�iB`'BkkBW$B7�B=B@4B%`B9B
��B
�1B
ϑB
̳B
�B
��B
��B
�EB
��B
�sB
��B
��B
��B
�MB
��B
u�B
X�B
B[B
3�B
�B
�B
	B
pB	�zB	�qB	�B	�-B	��B	��B	�OB	�XB	�B	�9B	�SB	�B	��B	��B	��B	�'B	�LB	��B	��B	��B	��B	��B	��B	�vB	�mB	��B	�B	i*B	?�B	[�B	.B	y�B	vFB	tB	jB	Y�B	EB	H1B	S�B	R�B	P}B	H�B	G_B	G�B	A B	0;B	B	#nB	$�B	�B��B��B��B�"B�B�B�vB��B��B��B�B�B��B�<B��B��B��B��B�GB��B��B��B�B�B�sB��B�&B�&B��B�B��B�Bp�B�1Br�Bt�Bl�Bh
Bb�Bk�BxBs3BffBL0B/�BKDBZBb�Ba�B`�BUBD�BG�BOvBF�B'�B_B9�BG+BDMB>wBAUBA;BCBBAB?.BH�BG�BC�B:�B/5B,=B*B6�B8�B6�B2�B"NB2B�B&�B.�B(�B�B 4B�B�B�B#�B2|B2|B3hB4TB2|B1[B-]B'mB�B�B��B�BdB*B'�B#�B!B$�B(�B%�B%�B)�B#B \B5tB8�B7�B6�B,�B 'B�B/�B6�B6�B5�B1�B3�B)*B$@B/B7�B8B=�B@�B:�B+�B+QB7�B;B5?B5ZB6`BD3BJ=BNBMBK�BG+B?.B>(B:^B/5B)BUMBQ�BI�BL�BW�Bg�Bj�Bh�Bk�Bo Bm�Bp�Bq'Bo5BmCBm]BxByXBzxB��B�vB��B��B�B��B��B��B��B�	B�SB�oB�;B�4B�TB�DB�UB�3B�+B��B��B��B��B��B��B��B��B��B��B�!B��B�B�FB�B�B�7B� B�4B�<B�rB�:B�B�QB�1B�WB�7B�EB�EB�MB�EB�YB�oB͹B�B�'BݘBٴB��BخB�B��B��B��B��B��B��B��B��B��B�B�+B�?B�>B�PB��B		RB	
rB	
rB	
rB	
rB		lB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"B	!�B	!HB	'RB	1AB	1[B	4TB	6`B	7fB	7fB	6`B	7fB	8lB	8lB	8lB	8lB	7�B	4�B	7�B	?�B	A�B	EB	K�B	L0B	P.B	QNB	]IB	]IB	]dB	\�B	`vB	_�B	_B	h�B	o B	p�B	v�B	y	B	|B	B	~BB	}VB	�OB	�3B	�?B	�KB	�KB	�KB	�XB	�^B	�^B	�xB	�xB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�
B	�0B	�0B	�B	�B	�0B	�=B	�"B	�=B	�WB	�IB	�;B	�;B	�5B	�OB	�UB	�?B	�TB	�nB	�nB	�MB	�|B	�aB	�ZB	�hB	��B	�|B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�DB	�B	�$B	�MB	�MB	�KB	�CB	�5B	�xB	�dB	�hB	�TB	�hB	�|B	ބB	ݘB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	��B	��B	�(B	�B	�B	�B	�B	�0B	�0B	�"B	�B
'B
B
AB
B
;B
 B	�HB	�HB
 B
 4B
;B
;B
3B
3B
9B
MB
EB
	7B
KB
_B
	RB
	RB
zB
�B

rB
^B
xB
^B
jB
jB
vB
�B
�B
�B
oB
�B
�B
�B
uB
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
B
 �B
!�B
!�B
"�B
"�B
#�B
#�B
#B
!�B
!�B
 'B
5B
!B
#B
$�B
$�B
'B
&�B
&�B
'B
'B
$B
$B
$B
'�B
(
B
(
B
)B
)�B
(
B
)*B
)B
)*B
,"B
,"B
-B
-B
-)B
-B
,"B
+B
*0B
+QB
+6B
+6B
+QB
,=B
/5B
2-B
2GB
2GB
2GB
1AB
1AB
0oB
1[B
1[B
2aB
4nB
6`B
8lB
9XB
8lB
7fB
5�B
3hB
5tB
6zB
8�B
9rB
9rB
7�B
9�B
<jB
;B
:�B
:�B
;�B
:�B
=�B
@�B
@�B
>�B
>�B
B�B
B�B
D�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
G�B
H�B
I�B
I�B
J�B
K�B
K�B
J�B
K�B
K�B
K�B
J�B
J�B
I�B
H�B
H�B
I�B
I�B
J�B
J�B
MB
MB
L�B
L�B
L�B
LB
M�B
N�B
N�B
NB
N�B
O�B
OB
M�B
N�B
OB
N�B
N�B
N�B
N�B
M6B
O(B
Q B
RB
RB
R B
TB
TB
TB
T,B
T�B
TB
R:B
S&B
S@B
UB
T,B
W?B
X+B
X+B
Y1B
ZB
\)B
[#B
[#B
[WB
Z7B
Y1B
ZQB
[=B
[=B
ZQB
\CB
]IB
]/B
]/B
\CB
\CB
\]B
[WB
[qB
_;B
_;B
^OB
^OB
]dB
\xB
]IB
_VB
_VB
_VB
^jB
_VB
`vB
`\B
`vB
bNB
bNB
abB
a|B
abB
bNB
cTB
cTB
cTB
cTB
cTB
bhB
b�B
abB
abB
bhB
bhB
`�B
bhB
b�B
cnB
dtB
ffB
ezB
ffB
ffB
ffB
ffB
ezB
f�B
e�B
f�B
gmB
g�B
g�B
f�B
ezB
e�B
hsB
hsB
iyB
iyB
i�B
i�B
iyB
iyB
hsB
g�B
g�B
g�B
jB
jB
j�B
jB
i�B
h�B
i�B
h�B
i�B
j�B
l�B
k�B
j�B
i�B
j�B
l�B
j�B
j�B
m�B
m�B
m�B
o�B
n�B
m�B
n�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
p�B
s�B
s�B
t�B
r�B
r�B
r�B
s�B
u�B
u�B
u�B
v�B
v�B
u�B
t�B
t�B
t�B
t�B
v�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901040034382019010400343820190104003438201901040200172019010402001720190104020017201901050024092019010500240920190105002409  JA  ARFMdecpA19c                                                                20181231093619  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181231003620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181231003624  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181231003624  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181231003625  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181231003625  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181231003625  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181231003625  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181231003625  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181231003626                      G�O�G�O�G�O�                JA  ARUP                                                                        20181231005554                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181231153626  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190103153438  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190103153438  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190103170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190104152409  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                