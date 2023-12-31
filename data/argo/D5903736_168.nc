CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041154  20190604094024  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�߼�+�|1   @�߽J�@3���n��d�/��w1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33Bϙ�B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  C   C�C�C�fC  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy]qD���D�N�D��
D�ٚD�{D�9�D��D���D��D�VD���D��qD��D�:�Dڌ�D��RD�	�D�:=D�~D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BυB��B��B��B��B��B��B��B��B�RB��B��B��C]C]C�)C��C	��C�)C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtWDyZ�D��D�MpD���D��RD�3D�8RD�~fD���D�GD�T�D���D��)D�{D�9GDڋ�D��
D��D�8�D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bNA�ffA�ffA�\)A�K�A�O�A�G�A�?}A�=qA�9XA�9XA�9XA�5?A�33A�33A�1'A�33A�33A�33A�33A�5?A�7LA�5?A�5?A�7LA�9XA�9XA�7LA�5?A�7LA�5?A�5?A�7LA�9XA�9XA�;dA�=qA�=qA�=qA�;dA�?}A�A�A�C�A�E�A�E�A�G�A�G�A�I�A�I�A�Q�A�v�A֑hA�C�A԰!A��/Aљ�A���A��TA�5?A���Aɕ�A��#A�%A��A��A���A��A���A��A���A�|�A�oA���A���A��A�M�A��#A�Q�A�?}A�\)A�E�A���A���A��A� �A�7LA��A�~�A���A��/A��7A�A�9XA��-A�=qA���A��FA�+A��wA�33A�p�A��uA�ƨA��A�A�-A��A���A�n�A�C�A��!A�K�A��A�?}A�|�A�G�A���A�hsA���A�O�A33A~  A}|�A}hsA|��AzbNAx�Aw�^AwoAv=qAs��Ar�Aqp�AoƨAn�AlQ�AkhsAj �Ahz�Ag��Ae&�Ab�HA`�jA_��A_�A^�RA]+A[��AZ�AW��AV�AS�AQ��AN��AL��ALI�AKXAI��AH�DAGAF^5AES�AD$�AB��AAK�A@M�A?
=A=A<�jA;?}A9p�A8bNA7C�A6bNA4�jA2��A1`BA0�/A0  A.ĜA-��A,��A*��A)�A)p�A(v�A'x�A%��A$�uA#��A#%A!�7A �DAXA�
A��AVA��AO�Av�A�`A�mA?}A�FAr�A�RA�wA�A��AjA��A
��A	��A�wA�AVA�DA\)Az�A�7A �A b@�5?@���@���@���@��R@��`@��
@�o@��+@��@��m@�x�@��;@�ff@���@���@�&�@�b@�@柾@�-@�`B@�A�@⟾@�E�@�p�@�@� �@�|�@���@ݺ^@�r�@��m@���@�O�@��@�J@�%@�z�@�I�@�\)@ҟ�@ёh@д9@ϥ�@���@��@�/@̓u@�t�@ʟ�@�V@�=q@�O�@ț�@�A�@�l�@�ȴ@��@��@��#@�hs@�%@ēu@��
@�\)@�"�@°!@�$�@��`@�j@���@��@���@�hs@�V@�(�@���@�S�@��+@��@���@�dZ@��T@��^@�&�@�Ĝ@�I�@��D@�A�@���@�33@��+@�hs@�@�=q@�~�@�33@� �@�l�@�$�@��-@��T@��@�J@��@�A�@�
=@��@��@��D@�9X@��@�ƨ@���@��P@�S�@���@���@��+@�ff@�M�@�@���@�hs@��/@���@�Q�@� �@�1@��
@���@�ƨ@���@��w@�|�@�S�@�K�@��@��H@���@�M�@�5?@�@��#@��h@���@�bN@�(�@���@���@��@��@��P@�dZ@�;d@��y@��+@�M�@�-@���@���@��h@��7@�p�@�G�@�&�@���@��@�  @���@���@�\)@�33@�33@�"�@�|�@��w@��@�t�@�S�@��@��@�-@�$�@�=q@���@�X@�/@�1'@��
@�ƨ@��F@���@��P@��w@���@�C�@��@�M�@�E�@�n�@��+@��\@��\@�v�@�M�@�$�@��^@�/@��D@�z�@�r�@�(�@���@��F@�dZ@�33@���@���@�v�@�$�@���@��@�p�@�O�@�?}@�/@�&�@���@���@�Ĝ@��u@�I�@� �@��;@���@�\)@��@���@��!@���@���@���@��+@�~�@�=q@�-@���@��T@���@�X@�?}@��@��`@���@��@��@�Z@���@��@�K�@�o@�v�@���@���@�;@��}@|��@rH�@h�Y@_6z@Y��@Qm]@L9X@C�@<�/@5+@-��@(�Y@# i@��@�@��@&�@�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�bNA�ffA�ffA�\)A�K�A�O�A�G�A�?}A�=qA�9XA�9XA�9XA�5?A�33A�33A�1'A�33A�33A�33A�33A�5?A�7LA�5?A�5?A�7LA�9XA�9XA�7LA�5?A�7LA�5?A�5?A�7LA�9XA�9XA�;dA�=qA�=qA�=qA�;dA�?}A�A�A�C�A�E�A�E�A�G�A�G�A�I�A�I�A�Q�A�v�A֑hA�C�A԰!A��/Aљ�A���A��TA�5?A���Aɕ�A��#A�%A��A��A���A��A���A��A���A�|�A�oA���A���A��A�M�A��#A�Q�A�?}A�\)A�E�A���A���A��A� �A�7LA��A�~�A���A��/A��7A�A�9XA��-A�=qA���A��FA�+A��wA�33A�p�A��uA�ƨA��A�A�-A��A���A�n�A�C�A��!A�K�A��A�?}A�|�A�G�A���A�hsA���A�O�A33A~  A}|�A}hsA|��AzbNAx�Aw�^AwoAv=qAs��Ar�Aqp�AoƨAn�AlQ�AkhsAj �Ahz�Ag��Ae&�Ab�HA`�jA_��A_�A^�RA]+A[��AZ�AW��AV�AS�AQ��AN��AL��ALI�AKXAI��AH�DAGAF^5AES�AD$�AB��AAK�A@M�A?
=A=A<�jA;?}A9p�A8bNA7C�A6bNA4�jA2��A1`BA0�/A0  A.ĜA-��A,��A*��A)�A)p�A(v�A'x�A%��A$�uA#��A#%A!�7A �DAXA�
A��AVA��AO�Av�A�`A�mA?}A�FAr�A�RA�wA�A��AjA��A
��A	��A�wA�AVA�DA\)Az�A�7A �A b@�5?@���@���@���@��R@��`@��
@�o@��+@��@��m@�x�@��;@�ff@���@���@�&�@�b@�@柾@�-@�`B@�A�@⟾@�E�@�p�@�@� �@�|�@���@ݺ^@�r�@��m@���@�O�@��@�J@�%@�z�@�I�@�\)@ҟ�@ёh@д9@ϥ�@���@��@�/@̓u@�t�@ʟ�@�V@�=q@�O�@ț�@�A�@�l�@�ȴ@��@��@��#@�hs@�%@ēu@��
@�\)@�"�@°!@�$�@��`@�j@���@��@���@�hs@�V@�(�@���@�S�@��+@��@���@�dZ@��T@��^@�&�@�Ĝ@�I�@��D@�A�@���@�33@��+@�hs@�@�=q@�~�@�33@� �@�l�@�$�@��-@��T@��@�J@��@�A�@�
=@��@��@��D@�9X@��@�ƨ@���@��P@�S�@���@���@��+@�ff@�M�@�@���@�hs@��/@���@�Q�@� �@�1@��
@���@�ƨ@���@��w@�|�@�S�@�K�@��@��H@���@�M�@�5?@�@��#@��h@���@�bN@�(�@���@���@��@��@��P@�dZ@�;d@��y@��+@�M�@�-@���@���@��h@��7@�p�@�G�@�&�@���@��@�  @���@���@�\)@�33@�33@�"�@�|�@��w@��@�t�@�S�@��@��@�-@�$�@�=q@���@�X@�/@�1'@��
@�ƨ@��F@���@��P@��w@���@�C�@��@�M�@�E�@�n�@��+@��\@��\@�v�@�M�@�$�@��^@�/@��D@�z�@�r�@�(�@���@��F@�dZ@�33@���@���@�v�@�$�@���@��@�p�@�O�@�?}@�/@�&�@���@���@�Ĝ@��u@�I�@� �@��;@���@�\)@��@���@��!@���@���@���@��+@�~�@�=q@�-@���@��T@���@�X@�?}@��@��`@���@��@��@�Z@���@��@�K�@�o@�v�@���G�O�@�;@��}@|��@rH�@h�Y@_6z@Y��@Qm]@L9X@C�@<�/@5+@-��@(�Y@# i@��@�@��@&�@�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�{B
��B
�{B
�{B
�uB
�uB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�hB
�hB
�hB
�oB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
�TB�TB	7BoB�B-B7LB:^BA�BB�By�B��Bs�BW
B^5Be`BhsBn�Bs�By�B��B��B�'B��B��B�B��B��B�dB��B��B�DB� B�B�7B�B{�Bx�B|�Bq�BcTB\)BXBQ�BK�B<jB/B'�B!�B{B+B��B�B�;BȴB�B}�BhsB_;BL�B;dB33B)�B{B
��B
�B
�ZB
��B
�qB
��B
��B
�\B
�7B
�+B
�B
s�B
jB
ffB
_;B
W
B
F�B
@�B
7LB
)�B
�B
JB
B	��B	�B	�B	�)B	��B	�qB	�FB	�'B	�B	��B	��B	�=B	u�B	hsB	[#B	N�B	A�B	;dB	<jB	=qB	6FB	/B	(�B	�B	�B	oB	DB	B��B��B�B�B�TB�)B�B��B��BǮBB�}B�qB�^B�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�VB�7B�1B�DB�%B�%B�7B�B�B�B�B�B�B�B� B� B�B� B}�Bz�By�By�Bx�Bx�Bx�By�Bw�Bv�Bw�Bw�Bx�Bx�Bw�Bw�Bx�By�Bz�Bz�B|�B|�B|�B|�B|�B|�B~�B�B�7B�=B�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�FB�XB�XB�XB�dB�qB�}BBĜBȴB��B��B��B��B�
B�B�B�B�)B�/B�HB�NB�TB�NB�BB�NB�HB�HB�TB�yB�B�B�B�B�B�B��B��B	B	+B	%B	%B	+B	B	B	B	B	\B	�B	&�B	)�B	+B	/B	49B	5?B	7LB	8RB	<jB	@�B	@�B	C�B	E�B	E�B	F�B	G�B	K�B	N�B	N�B	P�B	R�B	S�B	W
B	XB	YB	[#B	\)B	_;B	`BB	cTB	e`B	e`B	gmB	gmB	hsB	jB	k�B	l�B	n�B	n�B	q�B	r�B	t�B	v�B	w�B	y�B	z�B	z�B	}�B	~�B	�B	�B	�B	�%B	�+B	�1B	�1B	�=B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�FB	�^B	�dB	�jB	�qB	�wB	B	ĜB	ŢB	ǮB	ƨB	ƨB	ǮB	ƨB	ŢB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
�B
4B
B
$�B
1�B
<B
@ B
D�B
J=B
QNB
U�B
]/B
a�B
gmB
mB
p!B
t�B
y	B
~�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�&B
ނB�yBYB�B�B(1B2jB5~B<�B=�Bt�B��Bn�BR.BYPB`�Bc�Bi�Bn�Bt�B��B�B�DB��B��B�"B�	B��B��B�B��B�dB{B|*B�[B}.Bw	Bs�BxBl�B^zBWLBS4BMBF�B7�B*=B#B�B�BOB��B�B�_B��B�4By"Bc�BZjBG�B6�B.bB%+B�B
�%B
��B
ߍB
�B
��B
� B
��B
��B
�kB
�bB
|AB
n�B
e�B
a�B
ZwB
RHB
A�B
;�B
2�B
%6B
�B
�B
 \B	�,B	��B	�B	�hB	�	B	��B	��B	�jB	�PB	�B	��B	�}B	qB	c�B	VgB	JB	<�B	6�B	7�B	8�B	1�B	*`B	$AB	B	�B	�B	�B�MB� B�B��B��BޟB�uB�^B�=B�B��B��B��B��B��B��B�wB�jB�QB�JB�7B�$B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�wB�sB��B�qBhB|VB�qB�tBjB}bB{TB{RB|ZB{SByDBv5Bu/Bu,Bt'Bt'Bt(Bu/Bs BrBs!Bs#Bt$Bt)Bs!Bs"Bt&Bu-Bv7Bv0BxABxCBx@BxBBx@BxBBzNB|YB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�%B�5B�8B�QB�`B�gB�pB�tB��B��B��B��B��B��B��B��B��B��B��B�B�B�$B�DB�PB�]B�`B�]B�rB�zB�}BܘBݝBަBݠBۓBݟBܚBܖBަB��B��B��B��B��B��B�B�,B�KB�]B	{B	tB	uB	|B	 pB�[B�VB�iB	
�B	�B	"9B	%IB	&RB	*kB	/�B	0�B	2�B	3�B	7�B	;�B	;�B	>�B	@�B	@�B	A�B	B�B	GB	J%B	J&B	L5B	N@B	OBB	RUB	S_B	TfB	VtB	WvB	Z�B	[�B	^�B	`�B	`�B	b�B	b�B	c�B	e�B	f�B	g�B	i�B	i�B	l�B	m�B	pB	rB	sB	u)B	v.B	v+B	yAB	zGB	|RB	~`B	�nB	�tB	�wB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�4B	�CB	�NB	�SB	�WB	�eB	�gB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�8B	�IB	�GB	�NB	�PB	�XB	�kB	�tB	�nB	�pB	�sB	�wB	�{B	�{B	�|B	ڈB	ܔB	ݗB	ޞB	ߣB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�2B	�9B	�:B	�?B	�>B	�;B	�AB	�HB	�QB	�VB	�YB	�]B	�aB
 dG�O�B	�B
{B
�B
 	B
,�B
7dB
;HB
?�B
E�B
L�B
P�B
XuB
],B
b�B
hUB
kkB
o�B
tSB
zB
|B
,11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.005(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940242019060409402420190604094024  AO  ARCAADJP                                                                    20181121041154    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041154  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041154  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094024  IP                  G�O�G�O�G�O�                