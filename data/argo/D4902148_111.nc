CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-08-31T15:35:37Z creation;2017-08-31T15:35:39Z conversion to V3.1;2019-12-18T07:28:29Z update;2022-11-21T05:32:04Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170831153537  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA  I1_0397_111                     2C  Dd NAVIS_A                         0397                            ARGO 011514                     863 @�"��/ 1   @�"�hK� @;������d qu�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C�fC�fC���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/�3D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT�3DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�H 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�"�A�S�A��mA�ȴA۲-Aۗ�A�bNA�S�A�K�A�A�A��A���A�bA���A�ffA�K�AθRA�K�Aȏ\AǛ�A��`Aď\A��A��A�"�A�E�A��`A�1'A�5?A���A���A��uA��A�-A�5?A� �A��yA�`BA��9A�`BA�K�A�bA���A�7LA�x�A�XA���A�"�A�%A��A��wA�x�A�VA�33A�\)A���A��A���A��-A���A��A�JA�p�A���A���A�bA�ĜA�?}A���A�&�A���A��HA�A��!A���A��mA��!A�&�A�-A�
=A��A�/A���A��yA�I�A�JA��-A�r�A�9XA��/A��mA��A�33A��A�A��A�=qA���A��mA
=A}t�A|5?A{`BAzffAz�Ay�TAy��Ay��Ay�7Ay+Ax�AxQ�AvbNAtZAsAr�ApȴAn�!Ak�wAi�AgVAe�Ab~�Aal�A`�A_�TA_"�A^��A^�jA^ffA^{A]�PA]hsA];dA]VA\�A\(�A[�
A[x�AZ�!AZAYx�AX��AV��AU\)AS"�ARM�AQƨAQ`BAPĜAP�AO�#AO�FAO|�AOx�AOC�AO&�AN��AN�AL�RAK�PAJM�AIhsAG�mAEAD9XACXAB�HABI�ABbAA��A?ƨA>~�A>5?A=�#A<n�A;&�A:n�A8��A7�A6�\A6bA5VA4�+A4v�A4A2�\A1��A0��A-�TA+��A+|�A+l�A+\)A+K�A+/A*��A)ƨA(~�A(JA%�A%K�A%A$�uA#�-A"z�A ��A ZA��A�^A�A�AVA�/A��AƨA�A��A�A�An�A�AK�A�`A�!A�+AbNA�#A��A�PA�7A�Al�A/A��Az�AffA(�A��A
=A��AZA�
A�mA�;A��A�PAp�AO�A��A�;A
v�A
A	�PA	7LA�9A|�AȴA �A�A(�A��A7LA bN@�-@���@�9X@��@�?}@�ȴ@�=q@�{@��#@��m@��H@�x�@�@���@��@�@� �@߅@�ff@��`@��@��`@�dZ@ԣ�@��@У�@υ@ͩ�@˥�@�J@ț�@�1@��@ư!@�5?@��@Ł@�j@�"�@�V@��/@�Z@�9X@��@��@�dZ@�E�@�bN@���@���@��u@�
=@��-@�7L@�V@���@���@���@���@�n�@�5?@�$�@��@�l�@�V@��@��+@�5?@�p�@��@���@��7@�O�@��@��9@�r�@�I�@���@��@��@�5?@�{@��@��@�1@��@��!@���@���@���@���@��m@�K�@��@�ȴ@���@��@���@�n�@���@��@���@��m@��m@�o@�Ĝ@� �@�ƨ@�\)@�
=@��y@���@��R@���@���@�ȴ@��@�
=@��@�33@�+@��H@�v�@��^@�@���@�?}@��@�ƨ@�C�@��R@�5?@�@�X@��@�%@��@��/@��/@���@��u@�33@��+@�$�@��@�z�@��@���@��@�bN@�A�@�9X@�1'@�(�@�S�@�ff@��@���@�x�@��/@��/@��@��@��D@�1'@���@��;@��
@��@��m@��@��\@�^5@�J@��@��7@�V@���@��D@�1'@��@~��@~E�@}�T@}p�@}?}@}V@|�/@|�/@|��@|�j@|�j@|�@|��@|Z@{�m@{33@zM�@y�#@yhs@x��@xA�@w\)@u��@u�@t�/@t��@t��@t�D@tj@t(�@sƨ@r�@r-@q�@q�^@qx�@q7L@q%@p��@pĜ@p�u@p�9@p�9@p1'@pA�@o�@o��@o�;@o�;@o��@o�@o�P@o\)@oK�@o;d@n��@nE�@n{@m�@m@m`B@l��@l�j@l��@lZ@k�
@ko@j�!@j^5@i�7@i&�@h�u@h�@hQ�@h  @g�@g��@g�@g�P@g�P@g|�@gl�@gK�@g;d@g�@g
=@f�y@fȴ@f��@fff@f5?@e�@d�@d(�@c@bM�@a��@a��@a�7@a7L@a&�@a&�@`�u@`Q�@`b@_l�@_
=@^�@^�+@^ff@^E�@]�T@]?}@]�@\��@\Z@[dZ@Z^5@ZJ@Y�7@Y7L@Y%@Y%@Y%@Y%@Y%@Y%@X��@XbN@W;d@V�y@V��@VV@V@U�T@U��@Up�@UO�@U?}@T�/@T(�@S��@S�m@Sƨ@St�@SS�@S33@So@S@R�H@R��@R��@R��@R�!@R��@R�\@Rn�@RM�@R�@Q��@Q��@QG�@P�9@PA�@P  @O�@O+@N��@NV@M@L��@K�
@J~�@I�7@I7L@I%@H�`@HĜ@H��@H�@HA�@G�;@G��@G|�@G\)@GK�@G+@G+@G+@G�@Fȴ@FV@FV@FV@FE�@FE�@F$�@F{@E�@E�T@E@Ep�@D��@D�j@D�@D�D@Dj@C��@C�@C"�@B~�@A��@Ax�@A�@A%@A%@A%@@Ĝ@@A�@?�P@?+@>��@>$�@=�@=�-@=p�@=?}@=/@=V@<��@<��@<Z@<�@;��@;�
@;�
@;ƨ@;��@;t�@;C�@:��@:n�@:�@9��@9X@9�@8�u@8b@7�@6V@5��@5��@5p�@5?}@4��@4��@4��@4��@3��@3�@3t�@333@3"�@2�@2��@2��@1��@0�`@0�9@0�u@0r�@0Q�@01'@0 �@0  @/�;@/�@/|�@/+@.ff@.5?@.@-@-�h@,��@,�@,��@,��@,��@,��@,��@,��@,�D@,z�@,Z@,9X@,9X@,(�@,�@,1@,1@+��@+�
@+C�@+@*��@*��@*��@*�\@*=q@)��@)x�@)G�@)&�@)�@(�`@(�`@(��@(�9@(��@(��@(�@(r�@(A�@(  @'�;@'��@'��@'��@'��@'�P@'\)@'K�@'\)@';d@'�@'�@'+@'�@&��@&�R@&��@&v�@&E�@%�@%��@%�@%?}@$z�@$(�@#��@#ƨ@#t�@"�\@"�@!��@!7L@ �`@ ��@ �9@ Q�@ 1'@  �@ b@   @�;@�w@�w@|�@;d@�y@ȴ@�+@E�@$�@@@�@�T@��@@��@�@�@�@�@�@�@�@��@z�@9X@1@�m@�
@ƨ@�F@��@��@��@�@dZ@S�@dZ@C�@33@"�@"�@o@@�H@��@��@��@�!@��@��@�!@M�@��@Ĝ@|�@ff@5?@@�@��@@@�-@��@�@O�@�@�j@I�@9X@�m@��@t�@33@-@J@J@J@J@J@��@�@�#@�#@�#@��@�^@�^@�^@�^@��@��@��@��@�7@hs@X@%@�@|�@;d@�@�@��@@�@��@��@z�@9X@�@�@1@�m@�
@�
@�F@t�@o@
��@
�!@
^5@
=q@
-@
=q@
�@
-@
�@	hs@  @
=@�R@v�@ff@ff@V@V@E�@@��@?}@��@9X@9X@9X@�@�@�@1@��@��@�F@t�@"�@@@�!@M�@-@-@�@-@-@-@�@J@J@�@��@��@J@��@�@�#@��@�^@��@�7@�7@�7@x�@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�"�A�S�A��mA�ȴA۲-Aۗ�A�bNA�S�A�K�A�A�A��A���A�bA���A�ffA�K�AθRA�K�Aȏ\AǛ�A��`Aď\A��A��A�"�A�E�A��`A�1'A�5?A���A���A��uA��A�-A�5?A� �A��yA�`BA��9A�`BA�K�A�bA���A�7LA�x�A�XA���A�"�A�%A��A��wA�x�A�VA�33A�\)A���A��A���A��-A���A��A�JA�p�A���A���A�bA�ĜA�?}A���A�&�A���A��HA�A��!A���A��mA��!A�&�A�-A�
=A��A�/A���A��yA�I�A�JA��-A�r�A�9XA��/A��mA��A�33A��A�A��A�=qA���A��mA
=A}t�A|5?A{`BAzffAz�Ay�TAy��Ay��Ay�7Ay+Ax�AxQ�AvbNAtZAsAr�ApȴAn�!Ak�wAi�AgVAe�Ab~�Aal�A`�A_�TA_"�A^��A^�jA^ffA^{A]�PA]hsA];dA]VA\�A\(�A[�
A[x�AZ�!AZAYx�AX��AV��AU\)AS"�ARM�AQƨAQ`BAPĜAP�AO�#AO�FAO|�AOx�AOC�AO&�AN��AN�AL�RAK�PAJM�AIhsAG�mAEAD9XACXAB�HABI�ABbAA��A?ƨA>~�A>5?A=�#A<n�A;&�A:n�A8��A7�A6�\A6bA5VA4�+A4v�A4A2�\A1��A0��A-�TA+��A+|�A+l�A+\)A+K�A+/A*��A)ƨA(~�A(JA%�A%K�A%A$�uA#�-A"z�A ��A ZA��A�^A�A�AVA�/A��AƨA�A��A�A�An�A�AK�A�`A�!A�+AbNA�#A��A�PA�7A�Al�A/A��Az�AffA(�A��A
=A��AZA�
A�mA�;A��A�PAp�AO�A��A�;A
v�A
A	�PA	7LA�9A|�AȴA �A�A(�A��A7LA bN@�-@���@�9X@��@�?}@�ȴ@�=q@�{@��#@��m@��H@�x�@�@���@��@�@� �@߅@�ff@��`@��@��`@�dZ@ԣ�@��@У�@υ@ͩ�@˥�@�J@ț�@�1@��@ư!@�5?@��@Ł@�j@�"�@�V@��/@�Z@�9X@��@��@�dZ@�E�@�bN@���@���@��u@�
=@��-@�7L@�V@���@���@���@���@�n�@�5?@�$�@��@�l�@�V@��@��+@�5?@�p�@��@���@��7@�O�@��@��9@�r�@�I�@���@��@��@�5?@�{@��@��@�1@��@��!@���@���@���@���@��m@�K�@��@�ȴ@���@��@���@�n�@���@��@���@��m@��m@�o@�Ĝ@� �@�ƨ@�\)@�
=@��y@���@��R@���@���@�ȴ@��@�
=@��@�33@�+@��H@�v�@��^@�@���@�?}@��@�ƨ@�C�@��R@�5?@�@�X@��@�%@��@��/@��/@���@��u@�33@��+@�$�@��@�z�@��@���@��@�bN@�A�@�9X@�1'@�(�@�S�@�ff@��@���@�x�@��/@��/@��@��@��D@�1'@���@��;@��
@��@��m@��@��\@�^5@�J@��@��7@�V@���@��D@�1'@��@~��@~E�@}�T@}p�@}?}@}V@|�/@|�/@|��@|�j@|�j@|�@|��@|Z@{�m@{33@zM�@y�#@yhs@x��@xA�@w\)@u��@u�@t�/@t��@t��@t�D@tj@t(�@sƨ@r�@r-@q�@q�^@qx�@q7L@q%@p��@pĜ@p�u@p�9@p�9@p1'@pA�@o�@o��@o�;@o�;@o��@o�@o�P@o\)@oK�@o;d@n��@nE�@n{@m�@m@m`B@l��@l�j@l��@lZ@k�
@ko@j�!@j^5@i�7@i&�@h�u@h�@hQ�@h  @g�@g��@g�@g�P@g�P@g|�@gl�@gK�@g;d@g�@g
=@f�y@fȴ@f��@fff@f5?@e�@d�@d(�@c@bM�@a��@a��@a�7@a7L@a&�@a&�@`�u@`Q�@`b@_l�@_
=@^�@^�+@^ff@^E�@]�T@]?}@]�@\��@\Z@[dZ@Z^5@ZJ@Y�7@Y7L@Y%@Y%@Y%@Y%@Y%@Y%@X��@XbN@W;d@V�y@V��@VV@V@U�T@U��@Up�@UO�@U?}@T�/@T(�@S��@S�m@Sƨ@St�@SS�@S33@So@S@R�H@R��@R��@R��@R�!@R��@R�\@Rn�@RM�@R�@Q��@Q��@QG�@P�9@PA�@P  @O�@O+@N��@NV@M@L��@K�
@J~�@I�7@I7L@I%@H�`@HĜ@H��@H�@HA�@G�;@G��@G|�@G\)@GK�@G+@G+@G+@G�@Fȴ@FV@FV@FV@FE�@FE�@F$�@F{@E�@E�T@E@Ep�@D��@D�j@D�@D�D@Dj@C��@C�@C"�@B~�@A��@Ax�@A�@A%@A%@A%@@Ĝ@@A�@?�P@?+@>��@>$�@=�@=�-@=p�@=?}@=/@=V@<��@<��@<Z@<�@;��@;�
@;�
@;ƨ@;��@;t�@;C�@:��@:n�@:�@9��@9X@9�@8�u@8b@7�@6V@5��@5��@5p�@5?}@4��@4��@4��@4��@3��@3�@3t�@333@3"�@2�@2��@2��@1��@0�`@0�9@0�u@0r�@0Q�@01'@0 �@0  @/�;@/�@/|�@/+@.ff@.5?@.@-@-�h@,��@,�@,��@,��@,��@,��@,��@,��@,�D@,z�@,Z@,9X@,9X@,(�@,�@,1@,1@+��@+�
@+C�@+@*��@*��@*��@*�\@*=q@)��@)x�@)G�@)&�@)�@(�`@(�`@(��@(�9@(��@(��@(�@(r�@(A�@(  @'�;@'��@'��@'��@'��@'�P@'\)@'K�@'\)@';d@'�@'�@'+@'�@&��@&�R@&��@&v�@&E�@%�@%��@%�@%?}@$z�@$(�@#��@#ƨ@#t�@"�\@"�@!��@!7L@ �`@ ��@ �9@ Q�@ 1'@  �@ b@   @�;@�w@�w@|�@;d@�y@ȴ@�+@E�@$�@@@�@�T@��@@��@�@�@�@�@�@�@�@��@z�@9X@1@�m@�
@ƨ@�F@��@��@��@�@dZ@S�@dZ@C�@33@"�@"�@o@@�H@��@��@��@�!@��@��@�!@M�@��@Ĝ@|�@ff@5?@@�@��@@@�-@��@�@O�@�@�j@I�@9X@�m@��@t�@33@-@J@J@J@J@J@��@�@�#@�#@�#@��@�^@�^@�^@�^@��@��@��@��@�7@hs@X@%@�@|�@;d@�@�@��@@�@��@��@z�@9X@�@�@1@�m@�
@�
@�F@t�@o@
��@
�!@
^5@
=q@
-@
=q@
�@
-@
�@	hs@  @
=@�R@v�@ff@ff@V@V@E�@@��@?}@��@9X@9X@9X@�@�@�@1@��@��@�F@t�@"�@@@�!@M�@-@-@�@-@-@-@�@J@J@�@��@��@J@��@�@�#@��@�^@��@�7@�7@�7@x�@x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��BɺBǮBĜBBBÖBƨBȴBɺB��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��BɺBƨBɺBB�RB�?B�FB�RB�FB�9B�B�B��B��B��B��B��B��B��B��B�hB�VB�JB�DB�=B�1B�%B�B~�Bq�B]/BO�BI�B?}B1'B,B'�B!�B�BhB1B��B�B�B�fB�TB�HB�BǮB�wB�?B�'B��B�oBy�BhsBVBO�BG�B?}B<jB7LB49B/B�BhBPB%B
��B
��B
�sB
�5B
��B
ǮB
��B
�uB
�=B
�B
~�B
|�B
z�B
y�B
x�B
w�B
u�B
r�B
m�B
aHB
T�B
K�B
E�B
<jB
-B
�B

=B	��B	�B	�BB	�B	��B	��B	��B	ɺB	ȴB	ƨB	ĜB	��B	��B	�}B	�wB	�jB	�XB	�LB	�9B	�!B	�B	��B	��B	��B	�hB	�%B	�B	}�B	{�B	w�B	u�B	s�B	s�B	q�B	q�B	p�B	o�B	m�B	hsB	bNB	\)B	T�B	O�B	G�B	<jB	5?B	1'B	/B	,B	)�B	&�B	�B	�B	�B	oB	JB	B	B��B�B�B�B�yB�mB�`B�TB�5B�#B�B��BǮBƨBƨBƨBŢBŢBB�qB�RB�9B�B��B��B��B��B��B��B�oB�PB�=B�7B�7B�1B�+B�%B�B�B� B~�B}�Bz�Bw�Bu�Bu�Bt�Bs�Bs�Br�Bq�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bl�Bk�BjBhsBffBe`BdZBcTBcTBbNBaHB`BB_;B^5B\)B\)BZBYBW
BVBR�BO�BM�BL�BK�BI�BH�BG�BE�BC�BA�B>wB=qB=qB;dB:^B8RB49B/B(�B%�B$�B#�B"�B#�B!�B�B�B�B�B{B{BuBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B)�B+B,B,B+B,B.B.B/B/B1'B6FB5?B6FB7LB7LB8RB:^B?}BI�BM�BN�BN�BO�BO�BP�BP�BR�BS�BT�BVBVBVBXBXB]/BaHBffBl�Bl�Bm�Bn�Bp�Br�Bs�Bs�Bs�Bu�Bw�B{�B|�B}�B�B�=B�DB�JB�VB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�XB�dB�jB�wB��BĜBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�)B�;B�BB�HB�TB�`B�sB�B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	%B	+B	1B	PB	hB	�B	�B	!�B	$�B	(�B	(�B	(�B	)�B	)�B	+B	+B	+B	,B	,B	,B	,B	,B	-B	/B	0!B	1'B	33B	49B	6FB	9XB	>wB	E�B	G�B	H�B	H�B	I�B	I�B	I�B	J�B	K�B	O�B	R�B	S�B	S�B	T�B	T�B	VB	VB	XB	YB	\)B	dZB	gmB	iyB	iyB	k�B	l�B	m�B	n�B	n�B	o�B	p�B	p�B	p�B	q�B	u�B	u�B	v�B	v�B	x�B	y�B	z�B	z�B	{�B	}�B	�B	�B	�B	�+B	�7B	�=B	�=B	�DB	�PB	�PB	�VB	�\B	�bB	�bB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�FB	�XB	�XB	�^B	�jB	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
DB
JB
PB
PB
PB
PB
PB
\B
bB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
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
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
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
J�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
R�B
S�B
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
XB
XB
XB
YB
YB
YB
[#B
[#B
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
_;B
_;B
`BB
`BB
`BB
aHB
bNB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
jB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
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
r�B
r�B
r�B
r�B
s�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�DB�)B�B�SB��B��B��B�B��B��B�^B͟B��B��B�xB߾B�B�+BٚB�mBևBևB�BӏB҉B��B�@BΊB�rB̳B�B��B�zB��B�*B�lB�+B��B��B��B��B�2B��B��B�B�4B��B��B�BB��B��B��B�B��B��B�Bu�B`'BQ�BL0BB�B2aB-CB)yB#�B�BuB
�B�B��B�AB�B�@B�B��BɺB�OB�`B��B��B�
B}<BkkBWsBQ�BIB@4B=<B8B5tB1�B�B:B�B�B
��B
�B
�KB
�BB
�EB
��B
��B
��B
�^B
�B
cB
}<B
{B
z*B
y$B
xRB
v`B
s�B
o�B
c�B
V�B
MPB
G�B
?cB
0�B
�B
�B	�}B	�GB	�B	�#B	��B	бB	�0B	�	B	�B	�EB	�9B	��B	��B	��B	�B	�B	��B	��B	�?B	�B	��B	�yB	��B	��B	��B	�+B	��B	~�B	|�B	x�B	vB	tB	tB	q�B	q�B	qB	pUB	n�B	jKB	c�B	]�B	V�B	RB	J#B	>]B	6`B	1�B	/�B	,�B	+B	)B	;B	EB	mB	FB	�B	�B	-B��B�|B�iB��B�0B��B�B�,B�pB�/BٚB��B�1B��B��B��B�BƨB��B�B��B�zB��B��B��B�FB�|B��B��B�FB��B��B��B�lB��B��B�EB�9B��B�B�BHB{�BxlBv`BvBu%BtBtTBr�Bq�Bq�Bq�Bq�Bq'Bq[BqBo�BoBncBmwBl"Bk6Bi�Bh�Bg�Bd�Bc�Bc�Bb�BbhBa�B`�B^�B\�B\�B[#BZ�BXEBW�BVBQ�BN�BM�BM6BK^BIRBH�BF�BD�BB�B>�B=�B>B<�B;dB9�B6�B33B,=B&�B%�B$�B#�B%B#:B 'B	BB�BB�B�B�B�B�BBB�B�B�BBsB�BEB�B	B�BB�BB�B�BpB �B!HB#�B&�B*KB+6B,=B,qB+�B,�B.cB.}B/�B0;B2�B7�B6FB7B7�B7�B8�B:^B?.BI�BNBO(BOBBPBPHBQNBQ�BSuBTFBU�BV�BV�BV�BX�BYB^�BbBg8BmBl�Bm�Bn�Bp�Br�BtBt9BtnBvzBxRB|B}VB~�B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�&B�DB�B�=B�IB��B��B��B��B��B��B��B��B��B��B��B��B��B�"B�BB��B�VB�jBΊB�BB�B�1B�CB�VB�\B�bB�nB��B�*B�;B�B�B�8B�DB�B�B�B�B�BB�.B�B�B	 4B	uB	�B	SB	SB	�B	zB	�B	�B	�B	�B	B	"B	%FB	)DB	)*B	)*B	*B	*0B	+B	+B	+B	,"B	,"B	,=B	,"B	,=B	-CB	/iB	0oB	1[B	3hB	4�B	6�B	9�B	>�B	E�B	G�B	H�B	H�B	I�B	I�B	J	B	KB	LJB	PHB	SB	TB	TB	UB	UB	VB	V9B	X+B	YB	\CB	dtB	g�B	i�B	i�B	k�B	l�B	m�B	n�B	n�B	o�B	p�B	p�B	p�B	q�B	u�B	u�B	v�B	wB	y	B	y�B	z�B	{B	|B	~BB	�[B	�MB	�mB	�_B	�lB	�rB	�XB	�^B	�PB	��B	�pB	�vB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�$B	�$B	�6B	�=B	�CB	�;B	�aB	�MB	�TB	�ZB	�tB	�zB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	��B	ƨB	��B	��B	��B	�B	�B	�B	��B	��B	��B	� B	�4B	�B	�,B	�,B	�2B	�+B	�KB	�1B	�1B	�WB	�WB	�]B	�]B	�dB	�IB	�OB	�5B	�OB	�OB	�OB	�jB	�VB	�VB	�\B	�vB	�|B	�B	�B	�B	�B	��B	�B	��B	��B	��B	� B	�'B	�B	�B	�	B	�	B	��B	��B	�B	�B	��B	�B	�B	�B	�"B	�B	�B	��B	�B	�B	�(B
  B
 B
  B
  B
 B
 B
 4B
 B
;B
;B
AB
-B
3B
3B
MB
gB
SB
YB
zB
	�B
�B
dB
PB
PB
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
;B
!B
#B
$B
$B
#�B
$�B
%B
%B
%�B
&B
'B
(
B
(
B
($B
(
B
(
B
)*B
)_B
*0B
,=B
,"B
-)B
-CB
-CB
-)B
-)B
./B
./B
./B
.IB
/OB
1AB
1AB
1AB
2aB
2|B
3hB
49B
49B
49B
4TB
49B
49B
4TB
4TB
4nB
4TB
5ZB
5?B
5?B
5?B
5ZB
5ZB
5ZB
5tB
6`B
7�B
7fB
7fB
7�B
7�B
8�B
9�B
9rB
9rB
:^B
:xB
:^B
:^B
:xB
:xB
:^B
:�B
:xB
;B
;B
;B
;B
;�B
<jB
<jB
<�B
<�B
<jB
<�B
<�B
<�B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
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
KB
LB
L�B
L�B
NB
NB
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
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
O(B
PbB
QNB
S[B
T,B
UB
T�B
UB
T�B
VB
VB
VB
VB
V9B
V9B
VSB
W?B
X+B
X+B
X+B
Y1B
YKB
YB
[=B
[=B
[=B
[#B
[#B
[#B
[=B
[#B
[#B
[#B
\)B
\)B
\CB
\CB
\)B
\)B
\)B
\CB
\CB
\)B
\]B
\CB
\xB
]dB
]~B
_VB
_VB
`\B
`\B
`vB
a�B
b�B
c�B
c�B
dtB
dtB
dZB
dtB
dtB
dtB
e`B
ezB
ezB
e�B
f�B
f�B
f�B
gmB
g�B
gmB
g�B
g�B
g�B
g�B
h�B
j�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
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
r�B
r�B
r�B
r�B
s�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709110031572017091100315720170911003157202211182131412022111821314120221118213141201804031937082018040319370820180403193708  JA  ARFMdecpA19c                                                                20170901003506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170831153537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170831153538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170831153538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170831153539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170831153539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170831153539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170831153539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170831153539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170831153539                      G�O�G�O�G�O�                JA  ARUP                                                                        20170831155610                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170831153240  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20170910153157  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170910153157  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103708  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123141  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                