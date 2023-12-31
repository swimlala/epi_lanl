CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-06T09:16:06Z creation;2019-06-06T09:16:09Z conversion to V3.1;2019-12-18T07:14:56Z update;2022-11-21T05:28:55Z update;     
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
_FillValue                 �  ]x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ah   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190606091606  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_174                     2C  Dd9NAVIS_A                         0397                            ARGO 011514                     863 @���� 1   @�� b� @;�1���.�d9��Ft1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B��B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bg��Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CW��CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD�}D��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�}D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�I�D�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A�ffA�dZA�dZA�bNA�`BA�1'A���A�ȴA�r�A��A���A�bNA�/A��A�9XA�XA�jA�^5A�ffA��A��A�ffA�A�A�ȴA�C�A�p�A�5?A��!A�E�A�A�dZA���A��A��yA�r�A�~�A���A�\)A�;dA�v�A��9A�G�A��A��/A�  A��7A��A��RA��A�ȴA�|�A���A�%A�~�A���A��A�VA��A�VA��9A���A��PA�\)A���A��PA�VA��A�^A~E�A}+A|��A{�wAz9XAy+Ax �AwS�Aux�AtI�As��As7LAr��Aq��An�Al�DAj�HAi�wAip�AiK�Ah�yAh-Ag�Af��Af{Ae�^Ad��AdZAdbAc?}AcoAa�A`��A_x�A\��A\M�AZz�AY33AXQ�AW�;AWp�AVv�AT�RARM�AQ��AQ�-AQhsAQAPVANbNAM�AL�9AK�AK&�AJ��AI�AI�#AI�AHM�AG��AGoAEG�AC;dAB �AAA@�A=�hA=%A<��A<~�A;|�A;?}A;�A:ffA9��A9p�A9O�A8��A7?}A6JA6  A5�FA5S�A4�A3�A3dZA2��A1��A0��A/p�A.�uA-��A-�A,�A+��A*^5A)?}A(�9A(A$Q�A#7LA#+A#A"ĜA"�A"E�A!��A!A v�A�A9XAC�A��A�+A$�A�wAG�AĜA�mAp�A&�AĜAVA;dA �AA~�A�A��A��A~�AjA9XA��Ax�A��AE�A��A�A�A
^5A�uAXA/A%A��A5?A  A��At�AG�AVA�A�A�FAx�AdZA%A�A�^At�A�A 1'@���@�1@��@�@��h@�33@���@�~�@�5?@�{@���@�9@�o@���@���@�v�@��@��#@�Q�@���@�@�u@�@��@�v�@���@���@�bN@��
@�;d@�hs@ە�@�J@ج@�;d@�E�@���@�G�@� �@�dZ@ҧ�@��#@��@�bN@ϥ�@���@Η�@���@̴9@��@Ɂ@�z�@�|�@ư!@�-@��@��@���@�;d@��D@� �@�ȴ@�{@���@�p�@�&�@�bN@�{@�&�@��@��@��R@��+@�~�@�v�@�J@��@�bN@���@��\@��@��j@��
@�|�@�;d@���@�J@�hs@��@�I�@�ƨ@�~�@�`B@���@�1'@�dZ@�
=@���@�E�@���@�7L@���@���@��w@���@�S�@�C�@�;d@�+@�"�@�o@��y@���@�ff@���@��7@�x�@�p�@���@�A�@�l�@�ȴ@�-@��h@��@�(�@��P@��@�v�@�ff@�V@�$�@��#@��^@���@�`B@�G�@�V@���@���@���@��@�I�@�b@���@��F@���@�
=@�v�@�-@��^@�hs@���@�r�@�ƨ@���@�|�@�K�@�+@��@�
=@��H@��!@�=q@�J@�/@�%@���@�bN@�A�@�(�@��@��m@��F@�t�@�@�~�@�x�@�&�@��/@���@�1'@��;@�K�@��@�n�@��^@�hs@���@���@�z�@�I�@�@~�+@}�T@}��@}@}?}@|�@|�@|��@|j@|1@{��@z��@z-@y�@y�7@x��@xA�@x1'@w�@w�@w
=@v�y@v�@v��@vff@v@u�-@u��@u�h@u`B@u�@t�@t�j@t�@t��@tz�@tI�@s�
@so@r�H@r�H@r�@r��@r-@q��@qx�@qX@q�@p�`@p�9@pA�@p  @o�@n��@nv�@m��@m/@mV@l��@l�j@l��@lj@l9X@k�F@kt�@k33@j��@j^5@j=q@jJ@i��@i�7@i7L@h��@h�@h  @g\)@g\)@g;d@f�R@e��@e�@e`B@e?}@eV@d�@d��@d1@c33@b�H@b��@a�@a��@ahs@a7L@a&�@a&�@a&�@a&�@a&�@a�@`�`@`�u@`A�@` �@`  @_�@_�;@_�@_��@_l�@_;d@_�@_
=@^ȴ@^E�@]�T@]/@\(�@[t�@[o@Z��@Z��@Z�\@Z~�@Zn�@Z=q@Y��@Y�7@Yx�@Yhs@YG�@Y7L@Y�@Y%@X��@X�u@W|�@V��@V�+@V5?@U�@T�@T�@TZ@TZ@TI�@TI�@T9X@T9X@S�F@SdZ@St�@So@R^5@Q��@Q�@Q�#@Q��@Q�7@Q�@Q%@PĜ@PQ�@Pb@O\)@O�@N�R@Nv�@Nff@NE�@N@M�@M�@M�T@M�T@M��@M��@Mp�@M`B@M�@L�@L��@Lj@L9X@K33@J��@J=q@I�@I7L@HbN@G�@G\)@G;d@G
=@F�R@FE�@E��@E�@E�@D�D@DZ@DI�@C�m@C��@CdZ@B�\@A�#@AX@@��@@r�@@bN@@A�@@b@?�w@?\)@>�y@>�+@>E�@>{@=�T@=��@=�-@=�@=O�@=�@<�@<�@<�@;ƨ@;ƨ@;�
@;��@<1@;��@;�m@;�
@;ƨ@;ƨ@;t�@;S�@;@:��@9��@97L@9�@9%@9%@9�@9%@8  @7��@7;d@6V@5`B@5V@4�@4�@4�@4�@4��@4z�@41@333@2�!@2J@1��@1�^@1x�@1hs@1hs@1hs@1hs@1X@17L@1%@0�`@0Ĝ@0��@0�u@0r�@0A�@0A�@01'@0 �@0  @/��@/�P@/K�@/
=@.�y@.�@.�@.ȴ@.�R@.��@.$�@-�-@-�@,�@,j@,I�@,I�@+��@+��@+C�@+o@*�H@*n�@*=q@*�@)�^@)x�@)7L@(Ĝ@(r�@( �@'�;@'|�@'K�@&ȴ@&E�@&$�@&$�@%�@%��@%�-@%�h@%�@%�@%?}@%/@%/@%�@%V@$��@$��@%V@$�@$�@$�@$�@$�@$��@$�/@$��@$�@$�@#�F@#33@"�\@"=q@!hs@ ��@  �@�w@�P@+@��@�y@�@�R@��@��@��@��@�+@ff@V@V@@p�@�@p�@?}@�@��@��@z�@z�@j@Z@Z@I�@(�@��@o@�\@-@7L@�`@�9@r�@bN@A�@b@�;@�w@�w@��@�P@|�@l�@\)@��@�y@��@�+@�+@v�@v�@v�@�+@V@5?@@E�@�T@`B@(�@��@��@��@��@�m@�
@�F@�F@ƨ@�F@�F@��@��@�@t�@C�@33@�H@�!@�\@~�@~�@n�@=q@�@�@��@��@��@�7@hs@G�@G�@7L@&�@��@Ĝ@�u@Q�@1'@  @�@�;@�w@�P@�R@V@5?@{@��@�-@��@/@z�@I�@I�@9X@9X@�m@�m@ƨ@�@dZ@S�@C�@C�@33@o@@
�!@
n�@
M�@
-@	x�@��@�9@bN@A�@b@�@�w@+@�R@��@�+@v�@v�@ff@5?@$�@@�@��@�@O�@/@V@�@��@��@(�@��@C�@�@��@�!@�\@~�@^5@=q@-@�@�@�#@�^@��@x�@hs@X@G�@7L@&�@&�@&�@�@�@%@%@%@ ��@ ��@ ��@ �`@ ��@ ��@ ��@ ��@ ��@ �`@ ��@ ��@ �@ 1'?�|�?���?�I�?��m?���?�=q?��^?�x�?���?�Q�?�1'?�b?��?�l�?�+?��y?�ȴ?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A�ffA�dZA�dZA�bNA�`BA�1'A���A�ȴA�r�A��A���A�bNA�/A��A�9XA�XA�jA�^5A�ffA��A��A�ffA�A�A�ȴA�C�A�p�A�5?A��!A�E�A�A�dZA���A��A��yA�r�A�~�A���A�\)A�;dA�v�A��9A�G�A��A��/A�  A��7A��A��RA��A�ȴA�|�A���A�%A�~�A���A��A�VA��A�VA��9A���A��PA�\)A���A��PA�VA��A�^A~E�A}+A|��A{�wAz9XAy+Ax �AwS�Aux�AtI�As��As7LAr��Aq��An�Al�DAj�HAi�wAip�AiK�Ah�yAh-Ag�Af��Af{Ae�^Ad��AdZAdbAc?}AcoAa�A`��A_x�A\��A\M�AZz�AY33AXQ�AW�;AWp�AVv�AT�RARM�AQ��AQ�-AQhsAQAPVANbNAM�AL�9AK�AK&�AJ��AI�AI�#AI�AHM�AG��AGoAEG�AC;dAB �AAA@�A=�hA=%A<��A<~�A;|�A;?}A;�A:ffA9��A9p�A9O�A8��A7?}A6JA6  A5�FA5S�A4�A3�A3dZA2��A1��A0��A/p�A.�uA-��A-�A,�A+��A*^5A)?}A(�9A(A$Q�A#7LA#+A#A"ĜA"�A"E�A!��A!A v�A�A9XAC�A��A�+A$�A�wAG�AĜA�mAp�A&�AĜAVA;dA �AA~�A�A��A��A~�AjA9XA��Ax�A��AE�A��A�A�A
^5A�uAXA/A%A��A5?A  A��At�AG�AVA�A�A�FAx�AdZA%A�A�^At�A�A 1'@���@�1@��@�@��h@�33@���@�~�@�5?@�{@���@�9@�o@���@���@�v�@��@��#@�Q�@���@�@�u@�@��@�v�@���@���@�bN@��
@�;d@�hs@ە�@�J@ج@�;d@�E�@���@�G�@� �@�dZ@ҧ�@��#@��@�bN@ϥ�@���@Η�@���@̴9@��@Ɂ@�z�@�|�@ư!@�-@��@��@���@�;d@��D@� �@�ȴ@�{@���@�p�@�&�@�bN@�{@�&�@��@��@��R@��+@�~�@�v�@�J@��@�bN@���@��\@��@��j@��
@�|�@�;d@���@�J@�hs@��@�I�@�ƨ@�~�@�`B@���@�1'@�dZ@�
=@���@�E�@���@�7L@���@���@��w@���@�S�@�C�@�;d@�+@�"�@�o@��y@���@�ff@���@��7@�x�@�p�@���@�A�@�l�@�ȴ@�-@��h@��@�(�@��P@��@�v�@�ff@�V@�$�@��#@��^@���@�`B@�G�@�V@���@���@���@��@�I�@�b@���@��F@���@�
=@�v�@�-@��^@�hs@���@�r�@�ƨ@���@�|�@�K�@�+@��@�
=@��H@��!@�=q@�J@�/@�%@���@�bN@�A�@�(�@��@��m@��F@�t�@�@�~�@�x�@�&�@��/@���@�1'@��;@�K�@��@�n�@��^@�hs@���@���@�z�@�I�@�@~�+@}�T@}��@}@}?}@|�@|�@|��@|j@|1@{��@z��@z-@y�@y�7@x��@xA�@x1'@w�@w�@w
=@v�y@v�@v��@vff@v@u�-@u��@u�h@u`B@u�@t�@t�j@t�@t��@tz�@tI�@s�
@so@r�H@r�H@r�@r��@r-@q��@qx�@qX@q�@p�`@p�9@pA�@p  @o�@n��@nv�@m��@m/@mV@l��@l�j@l��@lj@l9X@k�F@kt�@k33@j��@j^5@j=q@jJ@i��@i�7@i7L@h��@h�@h  @g\)@g\)@g;d@f�R@e��@e�@e`B@e?}@eV@d�@d��@d1@c33@b�H@b��@a�@a��@ahs@a7L@a&�@a&�@a&�@a&�@a&�@a�@`�`@`�u@`A�@` �@`  @_�@_�;@_�@_��@_l�@_;d@_�@_
=@^ȴ@^E�@]�T@]/@\(�@[t�@[o@Z��@Z��@Z�\@Z~�@Zn�@Z=q@Y��@Y�7@Yx�@Yhs@YG�@Y7L@Y�@Y%@X��@X�u@W|�@V��@V�+@V5?@U�@T�@T�@TZ@TZ@TI�@TI�@T9X@T9X@S�F@SdZ@St�@So@R^5@Q��@Q�@Q�#@Q��@Q�7@Q�@Q%@PĜ@PQ�@Pb@O\)@O�@N�R@Nv�@Nff@NE�@N@M�@M�@M�T@M�T@M��@M��@Mp�@M`B@M�@L�@L��@Lj@L9X@K33@J��@J=q@I�@I7L@HbN@G�@G\)@G;d@G
=@F�R@FE�@E��@E�@E�@D�D@DZ@DI�@C�m@C��@CdZ@B�\@A�#@AX@@��@@r�@@bN@@A�@@b@?�w@?\)@>�y@>�+@>E�@>{@=�T@=��@=�-@=�@=O�@=�@<�@<�@<�@;ƨ@;ƨ@;�
@;��@<1@;��@;�m@;�
@;ƨ@;ƨ@;t�@;S�@;@:��@9��@97L@9�@9%@9%@9�@9%@8  @7��@7;d@6V@5`B@5V@4�@4�@4�@4�@4��@4z�@41@333@2�!@2J@1��@1�^@1x�@1hs@1hs@1hs@1hs@1X@17L@1%@0�`@0Ĝ@0��@0�u@0r�@0A�@0A�@01'@0 �@0  @/��@/�P@/K�@/
=@.�y@.�@.�@.ȴ@.�R@.��@.$�@-�-@-�@,�@,j@,I�@,I�@+��@+��@+C�@+o@*�H@*n�@*=q@*�@)�^@)x�@)7L@(Ĝ@(r�@( �@'�;@'|�@'K�@&ȴ@&E�@&$�@&$�@%�@%��@%�-@%�h@%�@%�@%?}@%/@%/@%�@%V@$��@$��@%V@$�@$�@$�@$�@$�@$��@$�/@$��@$�@$�@#�F@#33@"�\@"=q@!hs@ ��@  �@�w@�P@+@��@�y@�@�R@��@��@��@��@�+@ff@V@V@@p�@�@p�@?}@�@��@��@z�@z�@j@Z@Z@I�@(�@��@o@�\@-@7L@�`@�9@r�@bN@A�@b@�;@�w@�w@��@�P@|�@l�@\)@��@�y@��@�+@�+@v�@v�@v�@�+@V@5?@@E�@�T@`B@(�@��@��@��@��@�m@�
@�F@�F@ƨ@�F@�F@��@��@�@t�@C�@33@�H@�!@�\@~�@~�@n�@=q@�@�@��@��@��@�7@hs@G�@G�@7L@&�@��@Ĝ@�u@Q�@1'@  @�@�;@�w@�P@�R@V@5?@{@��@�-@��@/@z�@I�@I�@9X@9X@�m@�m@ƨ@�@dZ@S�@C�@C�@33@o@@
�!@
n�@
M�@
-@	x�@��@�9@bN@A�@b@�@�w@+@�R@��@�+@v�@v�@ff@5?@$�@@�@��@�@O�@/@V@�@��@��@(�@��@C�@�@��@�!@�\@~�@^5@=q@-@�@�@�#@�^@��@x�@hs@X@G�@7L@&�@&�@&�@�@�@%@%@%@ ��@ ��@ ��@ �`@ ��@ ��@ ��@ ��@ ��@ �`@ ��@ ��@ �@ 1'?�|�?���?�I�?��m?���?�=q?��^?�x�?���?�Q�?�1'?�b?��?�l�?�+?��y?�ȴ?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111E�B1B1B1B1B1B
=B�B�B�BoBVBB�B�ZB�B��B�}B�FB�9B�!B��B��B�DB�%B� Bw�Bo�BiyBffBbNB^5BW
BE�B:^B2-B"�BJBB��B�B�NB�B��B��B�-B��B�uB}�Bq�BjBdZBZBJ�BC�B5?B�BVB%B
��B
��B
��B
�`B
��B
��B
ĜB
�RB
��B
�DB
�B
x�B
t�B
n�B
dZB
]/B
T�B
N�B
B�B
:^B
5?B
2-B
/B
&�B
oB
B	��B	�B	�B	�B	�B	�fB	�5B	�#B	�
B	��B	��B	��B	ɺB	ĜB	B	�jB	�9B	�B	��B	��B	�DB	�B	}�B	z�B	v�B	p�B	ffB	[#B	YB	XB	VB	R�B	Q�B	M�B	G�B	E�B	A�B	<jB	:^B	6FB	5?B	33B	/B	,B	'�B	�B	oB	DB	B��B�B�B�B�yB�ZB�NB�BB�)B�B�B�B�B��BȴBɺBȴBɺBĜBB�}B�dB�dB�RB�-B�!B�B�B��B��B��B��B��B�uB�7B�B�B�B�B�B~�B|�Bz�Bx�Bu�Bq�Bn�Bm�Bk�BjBiyBgmBe`BcTBbNBaHB`BB^5B\)BXBS�BO�BN�BL�BK�BJ�BJ�BJ�BI�BG�BF�BD�BB�B?}B<jB8RB6FB49B33B2-B1'B0!B0!B/B/B.B.B-B.B.B.B-B-B-B,B,B+B)�B&�B%�B �B�B�B�B�B�B�B�B�BuBuBuBoBoBoBhBbB\B\BVBVBVBVBPBPBPBPBJBJBDBDBDBJBJBJBJBJBJBJBPBPBPBPBPBPBPBPB\BbBbBhBhBhBuB{B�B�B�B$�B&�B(�B(�B)�B(�B)�B/B1'B33B6FB2-B7LB7LB7LB7LB:^B<jB=qBA�BB�BG�BJ�BK�BK�BN�BP�BR�BW
BXBYB^5BcTBdZBgmBjBl�Bn�Bo�Bq�Bs�Bt�Bu�By�Bz�B{�B{�B{�B{�B{�B|�B|�B}�B~�B�B�B�B�B�%B�1B�JB�\B�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�'B�-B�3B�?B�?B�FB�XB�qB�wB��BÖBƨB��B��B��B��B��B��B�B�B�
B�B�)B�/B�ZB�`B�sB�B�B�B�B�B�B�B��B��B	  B	B	+B	
=B	VB	hB	�B	�B	�B	%�B	)�B	-B	0!B	2-B	49B	6FB	<jB	?}B	?}B	?}B	A�B	C�B	C�B	E�B	F�B	H�B	J�B	N�B	O�B	P�B	Q�B	W
B	YB	ZB	\)B	]/B	`BB	bNB	cTB	dZB	e`B	gmB	iyB	jB	jB	l�B	m�B	o�B	p�B	p�B	q�B	r�B	r�B	t�B	v�B	w�B	w�B	w�B	w�B	y�B	z�B	|�B	|�B	~�B	~�B	� B	�B	�B	�B	�%B	�1B	�JB	�\B	�bB	�hB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�-B	�FB	�^B	�jB	�qB	��B	B	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
+B
1B
	7B
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
"�B
$�B
%�B
%�B
'�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
.B
/B
0!B
1'B
1'B
1'B
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
49B
49B
49B
49B
49B
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
8RB
8RB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
K�B
L�B
M�B
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
O�B
O�B
O�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
VB
W
B
XB
XB
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
\)B
\)B
\)B
\)B
\)B
\)B
]/B
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
e`B
e`B
e`B
e`B
e`B
e`B
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
iyB
jB
jB
jB
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
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
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
|�B
}�B
}�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111E�B1BKBfB�B	�B�B \B�B�B,B�B�B�FB�RB��BӏB��B��B�B�B��B��B��B�1B��BzDBp�BjBg�BcnB_�BYeBGzB;�B4�B%�B�B�B��B�AB�BۦB��B�aB��B��B��BHBr�Bk�Be�B\)BL~BFB8�B�B�BEB
��B
��B
�RB
��B
�MB
��B
��B
��B
�/B
�B
�GB
y�B
u�B
pUB
e�B
^�B
VSB
P�B
C�B
;0B
5�B
2�B
1B
*KB
B
�B	��B	�3B	��B	�OB	�B	�B	��B	��B	רB	��B	�}B	�dB	ʦB	�SB	�B	�B	�FB	��B	��B	��B	��B	�B	~�B	{�B	xRB	r�B	h�B	[�B	YeB	X�B	V�B	TFB	TB	OBB	HfB	F�B	BuB	="B	;0B	6�B	5�B	4�B	0B	-wB	*KB	!B	B	�B	�B�wB�oB�B�=B�B��B��B�-B��BٴBؓB�7B׍B�B��B�=B�lBʦBŢB�aB��B��B��B��B�hB�B��B�"B��B�zB�'B��B�_B�$B�rB�MB�gB�{B��B��B�B~B{�BzDBwfBr�Bo5Bn/Bl"Bk6Bj0BhXBf�BdBb�Ba�Ba-B_�B]�BZBV�BP�BOvBM�BLJBKBK)BKxBJXBH�BG�BE�BC�BAUB>�B:�B7�B4�B3�B2�B1�B0�B0�B/�B/�B.�B.�B-�B.}B.}B.cB-�B./B-�B,�B,�B,WB+6B)DB(
B!�B�BOBkB?B�B�B�BSBFB�B�B�B�B�BoBNB.BB�B�B�B�B�B�B�BB�B�BJB0B0B�B�B�BB�B�B�B�B�B�B�B�B�B<BpBHBBB B B�B�BB�B�B/B%�B'mB)_B)DB*B)�B+�B/�B1�B3�B6zB2|B7fB7�B7�B8B:�B=B>BBB'BC{BHKBKBLBLJBO\BQ�BS�BWYBX�BZB^�Bc�Bd�Bh
Bj�Bl�Bo BpBrBtBu%Bv`BzB{B|B|B{�B|B|B}"B}<B~BB}B�UB�-B�GB��B��B��B��B��B��B�B�B�/B�HB�&B��B�B�B�$B�0B�6B�6B�=B�CB�IB�OB�;B�UB�[B�aB�hB�tB�tB��B��B��B��B��B��B�B�)B�B�B�&B�B�B�B�9B�?B�B�xBݲB�tB�B��B�B�B��B�B��B�B�B�FB�rB	 OB	aB	zB	
�B	�B	�B	�B	B	;B	&2B	*KB	-]B	0UB	2aB	4�B	6�B	<�B	?�B	?�B	?�B	A�B	C�B	C�B	E�B	F�B	H�B	KB	OB	PB	QB	R:B	W
B	Y1B	Z7B	\]B	]dB	`vB	bNB	cnB	d�B	e�B	g�B	i�B	j�B	j�B	l�B	m�B	o�B	p�B	p�B	q�B	r�B	r�B	t�B	v�B	w�B	w�B	w�B	xB	zB	{B	}B	}B	B	B	�4B	�'B	�GB	�gB	�YB	��B	�~B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�8B	�0B	�B	�B	�;B	�;B	�AB	�[B	�|B	��B	�xB	��B	��B	��B	��B	ĶB	ƨB	ǔB	ǮB	ǔB	ȚB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�&B	�,B	�9B	�_B	�B	�]B	�OB	�VB	�\B	�\B	�bB	�|B	�bB	�B	�nB	�ZB	�ZB	�zB	�B	�zB	�fB	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	�B
  B
;B
uB
SB
YB
_B
�B
	�B
xB
~B
dB
jB
jB
�B
�B
�B
�B
jB
pB
pB
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
#�B
#B
%B
&B
&2B
($B
*B
*B
+B
+B
+B
+B
+6B
+6B
,WB
.IB
/5B
0UB
1AB
1AB
1'B
1'B
1'B
1B
1AB
1'B
2aB
2GB
2-B
3MB
33B
3MB
3MB
33B
49B
4TB
4TB
4TB
4TB
5ZB
5ZB
6`B
6FB
6FB
6FB
6`B
6`B
6zB
7fB
8lB
8�B
:xB
;�B
;B
;B
;B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
LB
MB
M�B
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
O�B
PB
PB
QB
Q�B
Q B
Q B
RB
RB
Q�B
R�B
R�B
R�B
R�B
R�B
SB
S&B
S&B
T,B
U2B
U2B
VSB
W$B
X+B
X+B
YB
Y1B
Y1B
Y1B
ZB
ZB
ZB
Z7B
ZB
ZB
Z7B
Z7B
[=B
[=B
\)B
\B
\)B
\)B
\B
\)B
\]B
\CB
\CB
\B
\CB
\xB
]�B
_VB
_;B
_;B
_;B
_VB
_;B
_;B
`'B
`BB
`\B
`BB
`BB
`BB
`BB
`\B
`\B
`\B
`vB
abB
abB
aHB
aHB
abB
abB
b4B
bNB
bhB
bhB
bNB
bhB
cTB
cTB
cTB
cTB
cnB
cnB
cnB
dZB
dZB
dtB
ezB
e`B
ezB
ezB
ezB
e�B
g�B
gmB
g�B
g�B
h�B
h�B
h�B
h�B
i�B
jB
jeB
jB
i�B
jB
j�B
jB
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
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
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
{B
{0B
}B
~B
~B
~(B
~�B
�B
�4B
�B
� B
�B
� B
�;B
� B
�B
�B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906030031302019060300313020190603003130202211182139042022111821390420221118213904201906040015252019060400152520190604001525  JA  ARFMdecpA19c                                                                20190606181459  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190606091606  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190606091608  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190606091608  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190606091609  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190606091609  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190606091609  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190606091609  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190606091609                      G�O�G�O�G�O�                JA  ARUP                                                                        20190606101515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190523153143  CV  JULD            G�O�G�O�F� �                JM  ARCAJMQC2.0                                                                 20190602153130  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190602153130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190603151525  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123904  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                