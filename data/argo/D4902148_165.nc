CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-02-24T12:37:50Z creation;2019-02-24T12:37:54Z conversion to V3.1;2019-12-18T07:16:48Z update;2022-11-21T05:29:21Z update;     
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
resolution        =���   axis      Z        d  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  M`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20190224123750  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_165                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @ة�����1   @ة�o� @<qo hی�d�hr�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
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
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN)CP)CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�C�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�$�A�"�A�+A� �A���A�A��A��uA�~�A�dZA���A��mA��HA���A��wA���A�dZA�7LA�JA��A��;A��FA���A�ȴA�A��A��A�n�A�l�A�ffA�O�A�C�A�7LA�$�A��A��A��A��A�{A�A���A�A�A� �A��/A���A�dZA��A��A��!A���A�M�A��hA�E�A�~�A���A�&�A��A���A�7LA�t�A���A� �A�VA�7LA�bNA��A���A�hsA�=qA��DA��A�O�A�|�A/A~v�A~bA}dZA{x�Ayx�Ax��Aw��Av^5At��Ast�Ar{AqdZAq\)Aq
=Ap�9AoG�Ann�Am�-Al�yAkt�Aj9XAi�AgVAe�Ac��Ac7LAb��Ab�Aa�mA`E�A^��A]G�A\�RA\n�A\^5A\bA[��A[&�AYO�AW�^AW�AV��AV��AV��AVĜAVȴAV�/AV�uAU��AT��AT9XAR  AQ�PAQ�APJAO�ANbNAM��AM7LAL�!AL �AKx�AI�mAH��AH�jAH�RAH�\AHQ�AHJAG�AF��AF��AF(�AEp�AD9XAC+AB��AA��AA�;AA�wAA�FAAS�A?�A=K�A<(�A;p�A;
=A:�DA:9XA9t�A8A�A7�#A7A7A7�A5+A4��A4�/A4�uA4�\A4��A3�A2��A2�A1hsA0VA.��A-�hA,�RA,^5A+��A++A*�yA*�A*�uA*�A*M�A)dZA)S�A)C�A)/A(��A(ffA&I�A%�hA$�A" �A!`BA ��A 9XA  �A A�;AƨA|�A��A��A��AG�AVAĜA�uA�
A��A�+A��A&�A�HAn�A�AAr�A�A�AoA�A�uAr�A1Al�A�`A��AVA�uA�A�A
A�AZA�A�A�AȴA~�A=qA�A��A5?A��A��AI�A�;A��AC�A ȴA E�@�ȴ@�&�@���@��R@���@�G�@���@�@���@�33@��@�@��m@�V@�@�Q�@�l�@�h@蛦@�j@�I�@�b@���@���@�ƨ@�E�@�/@�1'@�@�hs@ޟ�@�t�@أ�@�I�@׮@Ձ@�I�@ӶF@��y@�&�@�j@� �@�t�@Η�@�M�@��@�9X@�o@ʇ+@�J@�&�@ǅ@���@��@�`B@ģ�@�;d@�{@�V@��;@�+@��@�7L@��/@�j@�(�@���@�\)@�"�@�~�@�t�@��+@��@��^@���@���@���@�J@��h@�/@�G�@�`B@�7L@�%@�V@�1'@�t�@��@���@��@��D@�z�@�1@��w@��@���@�S�@���@�n�@���@��`@���@��F@�J@���@��D@��@��!@��@���@��9@�Q�@��w@�
=@�v�@�{@��^@�/@���@���@���@�Z@�9X@�1'@��@��@�o@��\@�$�@�@���@�X@��@�I�@�b@���@�|�@���@�M�@���@�x�@�(�@��
@�C�@���@�^5@��^@���@��@�O�@��`@��u@�A�@��@�S�@��@�~�@�/@�Q�@�ƨ@�o@�5?@���@��@�p�@��@�&�@�7L@�7L@�/@��@��@�V@���@��9@��j@�Ĝ@���@��@�bN@�1'@� �@��
@�dZ@���@�J@�@���@��@���@��@�\)@��y@���@���@��+@�v�@�$�@��T@���@��-@��@�%@��/@���@�r�@�Q�@�9X@�(�@��@�  @�;@;d@~5?@{��@z��@z��@z^5@y��@yX@y%@xĜ@x�u@xbN@w�P@w;d@w;d@w
=@v�@v��@vE�@vE�@vE�@v5?@v{@u�T@u�-@up�@u�@t�@t�j@tZ@s�m@st�@sC�@r��@r^5@r=q@r�@rJ@rJ@rJ@rJ@q��@q�#@q�7@pr�@o
=@n{@m�-@m�h@m�@mp�@m?}@mV@l�/@l�j@l�@jM�@i�^@i��@i�7@i7L@hĜ@hb@g��@f�R@f{@f@e��@e�@e?}@dI�@cƨ@cdZ@c@b��@b�@a��@a�^@aX@`��@`�9@`bN@` �@_�@_K�@^ff@]��@]�@]/@\�j@\�j@\�D@\�D@\I�@[dZ@[@Z��@Z=q@Y�#@Y��@YG�@X��@XĜ@XbN@XA�@Xb@W
=@T��@T(�@T1@S��@S��@S�m@S�
@S�
@S�
@Sƨ@Sƨ@S��@S33@R~�@Q�@Q��@QG�@Q&�@P�`@P�@PbN@PbN@PQ�@PQ�@PQ�@PQ�@PbN@PQ�@PA�@P �@Pb@O�;@O|�@O\)@O�@N�@N�@N�R@N�+@Nff@NV@NE�@N{@M�-@M?}@L��@L�j@LZ@L(�@Kƨ@K�F@K��@K��@K��@K�@KdZ@KC�@Ko@J��@J~�@J�@I��@IG�@H��@H��@H�u@HA�@G��@G��@G�P@G;d@F��@F{@E��@EO�@D�@DZ@D9X@D�@C�m@CC�@B�@B��@BM�@BJ@A��@A�7@Ahs@AX@AG�@A7L@@��@@�@@A�@@  @?�P@?
=@>�+@=�@<�@;o@:�@:J@:J@9�#@9��@9��@9��@9��@9�7@9hs@9G�@9X@9X@9%@8��@8��@8A�@81'@81'@8A�@8A�@8Q�@8Q�@8A�@8A�@8A�@81'@8 �@8b@8  @7�w@7�@6�R@6$�@5�-@4�@4��@4�D@4j@4I�@41@3�
@3��@3�@2��@1��@1G�@0��@0�@0  @/��@/��@/+@.�y@.�R@.v�@.E�@.$�@.{@-�T@-p�@-�@-�@-V@,��@,��@,j@,9X@,1@+�m@+�
@+t�@+33@+@*�H@*��@*��@*�\@*n�@*�@)��@)�7@)x�@)hs@)X@)�@(��@(�9@(�9@(��@(�@(Q�@( �@( �@( �@(b@'�@'�@'|�@'l�@'\)@'K�@';d@'+@'�@'
=@&��@&�@&v�@&ff@&$�@%�T@%�-@%?}@$�@$�j@$��@$��@$j@$I�@$9X@$(�@$1@#�m@#��@#S�@#@"��@"n�@!��@!x�@!X@!7L@!�@!%@ Ĝ@ �u@ bN@  �@   @�@l�@K�@�@v�@$�@@��@�-@��@�h@�@�@`B@O�@�@�@�@1@�F@�@o@�H@�\@^5@M�@�@��@�^@X@�@%@Ĝ@Q�@ �@ �@ �@b@b@�@��@�w@�@�P@�P@�P@l�@+@��@��@5?@�h@?}@?}@?}@?}@?}@V@�@�/@��@�@��@Z@9X@1@��@ƨ@��@S�@33@"�@@�H@��@n�@M�@-@�@J@�@��@hs@&�@�@%@�`@Ĝ@�u@A�@b@��@;d@�y@��@v�@V@�@@�-@��@�h@p�@`B@`B@`B@`B@?}@/@�@��@�@�j@�@��@I�@��@ƨ@�F@��@S�@
�@
��@
�!@
n�@
J@	��@	�@	�#@	�#@	��@	��@	��@	��@	��@	�^@	�^@	��@	hs@	hs@	hs@	X@	&�@�`@�@bN@1'@�;@�@|�@l�@K�@�@
=@
=@�y@ȴ@�R@��@V@$�@{@{@{@{@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�$�A�"�A�+A� �A���A�A��A��uA�~�A�dZA���A��mA��HA���A��wA���A�dZA�7LA�JA��A��;A��FA���A�ȴA�A��A��A�n�A�l�A�ffA�O�A�C�A�7LA�$�A��A��A��A��A�{A�A���A�A�A� �A��/A���A�dZA��A��A��!A���A�M�A��hA�E�A�~�A���A�&�A��A���A�7LA�t�A���A� �A�VA�7LA�bNA��A���A�hsA�=qA��DA��A�O�A�|�A/A~v�A~bA}dZA{x�Ayx�Ax��Aw��Av^5At��Ast�Ar{AqdZAq\)Aq
=Ap�9AoG�Ann�Am�-Al�yAkt�Aj9XAi�AgVAe�Ac��Ac7LAb��Ab�Aa�mA`E�A^��A]G�A\�RA\n�A\^5A\bA[��A[&�AYO�AW�^AW�AV��AV��AV��AVĜAVȴAV�/AV�uAU��AT��AT9XAR  AQ�PAQ�APJAO�ANbNAM��AM7LAL�!AL �AKx�AI�mAH��AH�jAH�RAH�\AHQ�AHJAG�AF��AF��AF(�AEp�AD9XAC+AB��AA��AA�;AA�wAA�FAAS�A?�A=K�A<(�A;p�A;
=A:�DA:9XA9t�A8A�A7�#A7A7A7�A5+A4��A4�/A4�uA4�\A4��A3�A2��A2�A1hsA0VA.��A-�hA,�RA,^5A+��A++A*�yA*�A*�uA*�A*M�A)dZA)S�A)C�A)/A(��A(ffA&I�A%�hA$�A" �A!`BA ��A 9XA  �A A�;AƨA|�A��A��A��AG�AVAĜA�uA�
A��A�+A��A&�A�HAn�A�AAr�A�A�AoA�A�uAr�A1Al�A�`A��AVA�uA�A�A
A�AZA�A�A�AȴA~�A=qA�A��A5?A��A��AI�A�;A��AC�A ȴA E�@�ȴ@�&�@���@��R@���@�G�@���@�@���@�33@��@�@��m@�V@�@�Q�@�l�@�h@蛦@�j@�I�@�b@���@���@�ƨ@�E�@�/@�1'@�@�hs@ޟ�@�t�@أ�@�I�@׮@Ձ@�I�@ӶF@��y@�&�@�j@� �@�t�@Η�@�M�@��@�9X@�o@ʇ+@�J@�&�@ǅ@���@��@�`B@ģ�@�;d@�{@�V@��;@�+@��@�7L@��/@�j@�(�@���@�\)@�"�@�~�@�t�@��+@��@��^@���@���@���@�J@��h@�/@�G�@�`B@�7L@�%@�V@�1'@�t�@��@���@��@��D@�z�@�1@��w@��@���@�S�@���@�n�@���@��`@���@��F@�J@���@��D@��@��!@��@���@��9@�Q�@��w@�
=@�v�@�{@��^@�/@���@���@���@�Z@�9X@�1'@��@��@�o@��\@�$�@�@���@�X@��@�I�@�b@���@�|�@���@�M�@���@�x�@�(�@��
@�C�@���@�^5@��^@���@��@�O�@��`@��u@�A�@��@�S�@��@�~�@�/@�Q�@�ƨ@�o@�5?@���@��@�p�@��@�&�@�7L@�7L@�/@��@��@�V@���@��9@��j@�Ĝ@���@��@�bN@�1'@� �@��
@�dZ@���@�J@�@���@��@���@��@�\)@��y@���@���@��+@�v�@�$�@��T@���@��-@��@�%@��/@���@�r�@�Q�@�9X@�(�@��@�  @�;@;d@~5?@{��@z��@z��@z^5@y��@yX@y%@xĜ@x�u@xbN@w�P@w;d@w;d@w
=@v�@v��@vE�@vE�@vE�@v5?@v{@u�T@u�-@up�@u�@t�@t�j@tZ@s�m@st�@sC�@r��@r^5@r=q@r�@rJ@rJ@rJ@rJ@q��@q�#@q�7@pr�@o
=@n{@m�-@m�h@m�@mp�@m?}@mV@l�/@l�j@l�@jM�@i�^@i��@i�7@i7L@hĜ@hb@g��@f�R@f{@f@e��@e�@e?}@dI�@cƨ@cdZ@c@b��@b�@a��@a�^@aX@`��@`�9@`bN@` �@_�@_K�@^ff@]��@]�@]/@\�j@\�j@\�D@\�D@\I�@[dZ@[@Z��@Z=q@Y�#@Y��@YG�@X��@XĜ@XbN@XA�@Xb@W
=@T��@T(�@T1@S��@S��@S�m@S�
@S�
@S�
@Sƨ@Sƨ@S��@S33@R~�@Q�@Q��@QG�@Q&�@P�`@P�@PbN@PbN@PQ�@PQ�@PQ�@PQ�@PbN@PQ�@PA�@P �@Pb@O�;@O|�@O\)@O�@N�@N�@N�R@N�+@Nff@NV@NE�@N{@M�-@M?}@L��@L�j@LZ@L(�@Kƨ@K�F@K��@K��@K��@K�@KdZ@KC�@Ko@J��@J~�@J�@I��@IG�@H��@H��@H�u@HA�@G��@G��@G�P@G;d@F��@F{@E��@EO�@D�@DZ@D9X@D�@C�m@CC�@B�@B��@BM�@BJ@A��@A�7@Ahs@AX@AG�@A7L@@��@@�@@A�@@  @?�P@?
=@>�+@=�@<�@;o@:�@:J@:J@9�#@9��@9��@9��@9��@9�7@9hs@9G�@9X@9X@9%@8��@8��@8A�@81'@81'@8A�@8A�@8Q�@8Q�@8A�@8A�@8A�@81'@8 �@8b@8  @7�w@7�@6�R@6$�@5�-@4�@4��@4�D@4j@4I�@41@3�
@3��@3�@2��@1��@1G�@0��@0�@0  @/��@/��@/+@.�y@.�R@.v�@.E�@.$�@.{@-�T@-p�@-�@-�@-V@,��@,��@,j@,9X@,1@+�m@+�
@+t�@+33@+@*�H@*��@*��@*�\@*n�@*�@)��@)�7@)x�@)hs@)X@)�@(��@(�9@(�9@(��@(�@(Q�@( �@( �@( �@(b@'�@'�@'|�@'l�@'\)@'K�@';d@'+@'�@'
=@&��@&�@&v�@&ff@&$�@%�T@%�-@%?}@$�@$�j@$��@$��@$j@$I�@$9X@$(�@$1@#�m@#��@#S�@#@"��@"n�@!��@!x�@!X@!7L@!�@!%@ Ĝ@ �u@ bN@  �@   @�@l�@K�@�@v�@$�@@��@�-@��@�h@�@�@`B@O�@�@�@�@1@�F@�@o@�H@�\@^5@M�@�@��@�^@X@�@%@Ĝ@Q�@ �@ �@ �@b@b@�@��@�w@�@�P@�P@�P@l�@+@��@��@5?@�h@?}@?}@?}@?}@?}@V@�@�/@��@�@��@Z@9X@1@��@ƨ@��@S�@33@"�@@�H@��@n�@M�@-@�@J@�@��@hs@&�@�@%@�`@Ĝ@�u@A�@b@��@;d@�y@��@v�@V@�@@�-@��@�h@p�@`B@`B@`B@`B@?}@/@�@��@�@�j@�@��@I�@��@ƨ@�F@��@S�@
�@
��@
�!@
n�@
J@	��@	�@	�#@	�#@	��@	��@	��@	��@	��@	�^@	�^@	��@	hs@	hs@	hs@	X@	&�@�`@�@bN@1'@�;@�@|�@l�@K�@�@
=@
=@�y@ȴ@�R@��@V@$�@{@{@{@{@{@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B��B��B�wB�dB�dB�^B�XB�^B�XB�9B�-B�-B�!B�9B�RB�FB�3B�B�B�B��B�B�!B�'B�B��B��B�B�B�B��B��B�B�B�B�!B�!B�B�B��B��B��B�{B�\B�=B�B�Bz�BiyBF�BB�B�dB��B{�B_;BN�BD�B:^B/B�B
��B
�BB
��B
��B
ɺB
ŢB
�FB
�B
��B
��B
�VB
�B
}�B
y�B
t�B
ffB
YB
Q�B
J�B
B�B
9XB
,B
 �B
�B
"�B
"�B
�B
�B
hB
JB
+B	��B	�B	�yB	�B	ȴB	�dB	�?B	�9B	�3B	�B	��B	�{B	�7B	�B	�B	�DB	�VB	�JB	�%B	x�B	k�B	e`B	bNB	cTB	ffB	iyB	iyB	k�B	t�B	t�B	n�B	iyB	`BB	^5B	[#B	VB	T�B	P�B	O�B	N�B	K�B	H�B	E�B	@�B	<jB	;dB	;dB	;dB	:^B	8RB	5?B	0!B	/B	+B	&�B	$�B	 �B	�B	�B	�B	�B	�B	�B	
=B	B��B��B��B��B��B�B�B�B�B�B�mB�5B�/B�)B�B�B�B��B��BȴBƨBÖB�}B�dB�RB�LB�?B�3B�-B�'B�'B�!B�B�B�B�B��B��B��B��B��B��B�hB�VB�JB�DB�=B�=B�=B�7B�+B�B� B}�B|�B{�Bz�Bx�Bv�Br�Bo�Bn�Bl�Bk�BjBiyBgmBffBe`BdZBcTBbNBbNBaHB`BB^5B]/B[#BYBXBW
BT�BQ�BN�BN�BM�BL�BL�BK�BJ�BI�BH�BG�BF�BE�BD�BD�BC�BB�BA�B@�B?}B>wB=qB=qB<jB;dB:^B:^B9XB8RB7LB6FB49B5?B49B49B33B2-B1'B1'B0!B0!B0!B0!B/B/B0!B0!B/B.B-B-B-B,B+B)�B)�B)�B)�B)�B(�B(�B(�B)�B,B-B,B-B-B,B+B,B,B.B.B.B0!B2-B33B33B33B33B5?B6FB6FB6FB;dB;dB;dB?}BB�BD�BE�BH�BL�BL�BJ�BK�BL�BL�BO�BQ�BQ�BR�BVBXBZBZBZB[#B_;BcTBdZBdZBe`Be`BffBiyBjBjBk�Bl�Bl�BjBl�Bm�Bq�Bu�Bx�Bz�B}�B~�B� B�B�B�B�+B�=B�DB�DB�DB�PB�VB�VB�VB�VB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�LB�RB�^B�wB��BÖBÖBÖBĜBƨBȴB��B��B��B��B��B�/B�HB�NB�`B�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	%B	+B	DB	VB	�B	�B	"�B	"�B	$�B	%�B	&�B	)�B	,B	-B	.B	/B	2-B	2-B	33B	49B	5?B	5?B	6FB	6FB	7LB	7LB	9XB	<jB	G�B	L�B	L�B	L�B	N�B	O�B	O�B	Q�B	Q�B	R�B	XB	ZB	ZB	[#B	]/B	`BB	bNB	bNB	bNB	cTB	cTB	dZB	e`B	ffB	hsB	iyB	iyB	k�B	n�B	o�B	q�B	t�B	u�B	u�B	v�B	v�B	v�B	v�B	v�B	v�B	w�B	x�B	|�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�7B	�7B	�7B	�=B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�wB	��B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�`B	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
1B
1B

=B

=B
DB
DB
DB
PB
VB
\B
\B
bB
bB
hB
hB
hB
hB
hB
oB
uB
uB
{B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
(�B
(�B
+B
+B
+B
,B
,B
,B
-B
-B
,B
.B
/B
0!B
1'B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
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
G�B
G�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
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
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
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
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
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
jB
jB
jB
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
l�B
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
m�B
m�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B��B�aB�B��B��B��B��B��B�B�nB�GB�aB�oB��B��B��B��B�OB�=B�=B��B�B�;B�[B�cB�*B�B�B�)B�B�B�B�B�B�B�;B�;B��B��B��B�B�7B�B��B��B��B�AB}�Bn}BNpB�B��B��B�	B�4Ba�BP�BF�B<�B33B�B B
�B
�B
οB
��B
�B
��B
��B
��B
��B
�.B
��B
~�B
{0B
v�B
h�B
ZQB
S[B
LdB
DgB
:�B
-wB
!|B
�B
#TB
#�B
 BB
�B
oB
�B
�B	��B	�tB	��B	�B	�rB	�B	��B	��B	��B	�B	��B	��B	��B	��B	�aB	��B	��B	��B	�1B	zxB	l=B	e�B	bhB	c:B	ffB	i�B	i�B	l=B	u�B	u�B	pB	k�B	aB	_B	\]B	V�B	VmB	Q�B	P�B	O�B	L�B	I�B	G_B	A�B	<�B	;�B	;�B	;�B	:�B	9	B	6B	0�B	/�B	,"B	(sB	%�B	!|B	�B	�B	�B	�B	�B	+B	dB	{B��B�B��B��B��B��B�B��B��B�B�DBޞB�~BܒB�QBچB�QB�B��B��B�KBňB��B�jB��B�B��B��B�|B�[B�vB��B�B�=B�=B�kB��B�B�>B�B�jB�7B��B�BB��B�xB��B��B��B�#B�B��B�;B~wB}VB|jB{�Bz^By>BtTBp�BoOBmBl=BkkBj�Bh>BgBf2Bd�Bc�Bb�Bb�Ba�Ba-B_;B^�B\)BY�BX�BW�BW
BTBO�BO(BNpBMPBMPBLdBK�BJ�BI�BH�BG�BF�BE9BEBDMBCaBBuBA�B@�B?}B>(B>B=<B<�B;B;JB:�B9>B88B6�B5?B5�B5%B5B4TB2�B1AB1[B0UB0UB0UB0�B0!B/�B1B1AB0�B0!B/5B.�B-�B,�B,qB*�B*�B*�B+B*B)_B)�B)�B*B,�B-�B,�B-�B-�B,�B,"B,�B,�B.�B.�B/B1B2�B4B3�B3�B3�B5�B6�B6�B6�B;�B;�B<PBA;BC-BEBF%BIlBM�BM�BK^BL0BMBL�BO�BRBR BS@BV�BX�BZ�BZkBZ�B[�B_pBc�Bd�BdtBe�Be�Bf�Bi�BkBkBk�Bm]Bm�BkQBl�BnIBraBvFByXB{�B~]B}B��B�{B�mB�SB��B�rB�xB�xB��B�jB�pB��B��B��B��B��B��B��B�	B�B�B� B�:B�B�fB�eB�wB��B��B��B��B��B��B��B��B��B��B�B��B�B�0B�B�:B҉B��B��B��B��B��B�B�B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�(B	 4B	 B	[B	uB	�B	�B	mB	YB	�B	�B	�B	
B	B	"�B	#B	$�B	%�B	'8B	*0B	,"B	-CB	.IB	/iB	2GB	2aB	3hB	4TB	5ZB	5ZB	6`B	6`B	7�B	7�B	9�B	=<B	HB	L�B	MB	M6B	N�B	O�B	PB	RB	R B	S@B	X+B	ZB	Z7B	[=B	]IB	`\B	bNB	bhB	bhB	cTB	cTB	dtB	ezB	f�B	h�B	iyB	i�B	k�B	n�B	o�B	q�B	t�B	u�B	u�B	v�B	v�B	v�B	v�B	v�B	v�B	xB	yXB	}qB	�UB	�9B	�?B	�_B	�EB	�1B	�fB	�RB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�$B	�0B	�"B	�CB	�OB	�AB	�AB	�aB	�nB	�?B	�`B	�fB	��B	��B	��B	��B	��B	��B	ðB	ĜB	ĶB	żB	��B	��B	��B	��B	��B	�B	��B	�B	� B	�B	�B	�B	�,B	�{B	��B	�VB	�\B	�HB	�HB	�HB	�HB	�HB	�HB	�bB	�bB	�bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�<B	�B	�.B
 B
 B
 B
;B
B
-B
GB
MB
SB
_B
fB
fB

XB

XB
^B
^B
xB
jB
pB
vB
\B
}B
bB
�B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
B
B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%B
&B
'B
($B
)*B
)DB
+6B
+B
+B
,"B
,"B
,"B
-)B
-)B
,WB
.IB
/OB
0UB
1AB
2aB
3MB
33B
3�B
4TB
5?B
5tB
5ZB
6`B
6`B
6zB
6zB
7fB
8RB
8RB
8lB
8lB
8lB
9rB
9XB
:xB
:xB
:xB
:xB
;�B
;B
;dB
;B
<jB
<�B
<�B
<�B
=�B
=qB
=qB
>�B
>�B
>�B
?�B
?}B
?�B
?�B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
AoB
A�B
AoB
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
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
G�B
G�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
NB
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q B
Q B
Q B
Q B
Q B
R B
SB
SB
S&B
TB
TB
UB
UB
UB
UB
VB
V9B
W$B
W$B
W$B
W$B
X+B
XB
XB
XB
XB
X+B
YB
YB
YB
Y1B
YB
X�B
Y1B
YB
Z7B
ZQB
Z7B
[WB
\CB
\CB
\)B
\)B
\)B
\CB
]/B
]/B
]IB
]IB
]IB
]IB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_;B
_;B
`\B
`\B
`\B
`\B
aHB
aHB
aHB
abB
aHB
abB
bhB
b4B
bNB
bhB
bhB
cnB
cnB
cTB
dtB
d�B
ezB
ezB
ezB
f�B
ffB
gmB
gRB
gmB
gmB
g�B
gmB
gmB
hsB
gmB
g�B
hsB
hsB
h�B
hsB
h�B
hsB
h�B
h�B
i�B
i�B
i�B
jB
j�B
j�B
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
l�B
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
m�B
m�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903050033362019030500333620190305003336202211182138102022111821381020221118213810201903060020502019030600205020190306002050  JA  ARFMdecpA19c                                                                20190224213628  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190224123750  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190224123752  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190224123753  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190224123753  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190224123753  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190224123754  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190224123754  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190224123754  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190224123754                      G�O�G�O�G�O�                JA  ARUP                                                                        20190224125639                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190224153342  CV  JULD            G�O�G�O�F�M                JM  ARCAJMQC2.0                                                                 20190304153336  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190304153336  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190305152050  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123810  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                