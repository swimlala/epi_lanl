CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-09-29T07:28:55Z creation; 2023-04-26T19:14:26Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180929072855  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_026                 7316_008644_026                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؅p
@؅p
11  @؅=�c@؅=�c@+� �Ŭ@+� �Ŭ�d+����d+���11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @B�\@�G�@��\@\@޸R@��RA  A\)A*�HA@��A`  A~�RA�  A�Q�A�  A�  A�  A�  A�B   B  B(�B  B   B(  B0(�B8Q�B@(�BH(�BP  BX(�B`  Bh  Bp(�Bx(�B�=qB�{B��B��B��B��B�  B�  B�  B��B��B�{B�  B��B��B��B��B�  B�  B�  B�  B�  B�{B�  B��B��B��B�  B�{B�{B��B��C   C  C  C  C  C
  C
=C
=C
=C  C
=C
=C  C  C��C�C��C"  C$  C&  C({C*  C,  C-��C0  C2
=C4
=C5��C7��C:
=C<
=C>  C@  CB
=CD
=CF  CH  CJ  CL
=CN
=CP  CQ��CS��CV
=CX
=CZ
=C\  C^
=C_��Ca��Cd  Cf  Ch  Cj  Cl
=Cn
=Cp  Cq��Cs��Cv  Cx
=Cy��C{��C}��C�  C�C�C�  C���C���C�  C�C�C�C�  C�  C�C�  C���C�  C�C�C�  C�C�C�  C�  C�  C�  C�C���C���C�  C�  C�  C�C���C���C���C���C���C�  C�C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�C���C���C���C�  C�C�C���C���C�  C�  C���C�  C�C�C�
=C�C���C�  C�  C���C�  C�C�C�  C���C�  C���C�  C�
=C�C���C�  C�C�  C���C���C�  C���C���C�  C�  C���C�C�  C���C���C�  C�  C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�C���C�  C�C�  C�  C�C�C�C�C�  D   D � D  D}qD�qD}qD�qD}qD�qD}qD�qDz�D��Dz�D  D}qD�qD��D	  D	� D
  D
� D�D� D  D}qD�qD}qD�D��D��D}qD  D}qD�D�D�D� D  D��D�qD}qD�qD}qD�D��D  D� D�D}qD  D� D�D��D�qD� D�D� D�qD}qD  D�D�D� D�qD � D!�D!� D!�qD"� D#D#��D$  D$}qD$��D%z�D%��D&z�D&��D'z�D(  D(�D)�D)}qD)�qD*}qD+  D+� D+�qD,��D-�D-��D.  D.� D/  D/��D0�D0� D1  D1� D2  D2� D3  D3}qD4  D4� D5�D5��D5�qD6� D7D7}qD7�qD8��D9�D9� D9�qD:� D;�D;�D<�D<��D=  D=}qD=�qD>}qD?D?��D@�D@� DA  DA� DB  DB� DB��DC}qDD  DD� DE  DE}qDF  DF��DG  DG}qDG��DH}qDI  DI}qDJ  DJ�DK�DK� DK�qDL}qDL��DM� DN�DN� DO  DO}qDO�qDP}qDQ  DQ�DR�DR��DS�DS��DT�DT� DT�qDU� DV  DV}qDW  DW��DX  DX}qDY  DY��DZ�DZ}qDZ�qD[��D\�D\� D\��D]}qD^  D^� D_  D_� D`�D`��D`�qDa� Db�Db� Dc  Dc}qDc�qDdz�Dd�qDe��Df�Df� Dg  Dg}qDh  Dh� Dh�qDi}qDi��Dj}qDj�qDk}qDl  Dl� Dm�Dm�Dn�Dn� Do�Do��Do�qDp}qDp�qDq� Dr  Dr}qDs  Ds}qDs�qDt}qDu  Du� Dv  Dv}qDv�qDwz�Dw�qDx��Dy  Dy}qDz�Dz� Dz�qD{}qD{��D|}qD}  D}� D~  D~� D~�qD� D�  D�>�D�� D�� D���D�AHD��HD��HD�HD�@ D�~�D���D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D��HD��HD��D�AHD�� D���D���D�@ D��HD��HD�  D�AHD��HD���D�HD�@ D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�@ D�� D��HD�HD�>�D�� D�� D�  D�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D��qD���D�>�D�~�D�� D�  D�AHD�~�D���D���D�>�D�� D��HD�HD�@ D���D�D�HD�@ D�~�D��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�B�D��HD�� D�HD�AHD�~�D��qD��qD�=qD�~�D���D���D�@ D�� D��HD���D�@ D��HD���D���D�>�D�� D���D�HD�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD�~�D��HD�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD���D��HD�  D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D�� D��HD�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�>�D�~�D���D�  D�B�D���D�D��D�@ D�}qD�� D�  D�=qD�� D�� D�  D�AHD�� D��qD�  D�>�D�� D�� D�HD�AHD���D�D�HD�@ D�~�D�� D�HD�AHD��HD�� D�HD�AHD D¾�D�  D�@ DÁHD�� D�HD�AHDāHD��HD�HD�@ Dŀ D��HD�HD�AHDƁHD��HD�  D�>�Dǀ D��HD�  D�>�D�~�D�� D�  D�@ D�~�Dɾ�D�  D�>�Dʀ D��HD�  D�@ DˁHD��HD�HD�AHD�~�D̽qD���D�AHD́HD��HD�  D�AHD΀ D�� D���D�>�Dπ D��HD�  D�@ DЀ D�� D�HD�AHDр D�� D�HD�@ DҁHD�� D���D�@ DӀ D�� D�HD�B�DԁHD�� D�HD�@ DՀ D�� D���D�>�DցHD��HD�  D�@ DׁHD�� D�HD�@ D؀ Dؾ�D�  D�AHDفHD��HD�  D�>�Dڀ D�� D�HD�@ D�}qD۾�D���D�AHD܀ D��HD�HD�>�D݀ D�� D�  D�AHDހ D޽qD���D�>�D߀ D�� D���D�AHD�� DྸD���D�>�D�~�DᾸD���D�>�D� D�� D�  D�@ D� D��HD��D�AHD䂏D��HD��D�AHD�~�D�� D�HD�@ D�~�D�qD�  D�AHD� D�� D��D�AHD�HD�� D�  D�@ D�HD��HD�  D�>�D�}qD꾸D�  D�@ D�HD��HD�  D�@ D�~�D�� D���D�@ D�HD��HD�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D�  D�@ D�HD�� D���D�=qD�~�D�D�HD�AHD�HD�� D�HD�AHD�HD��HD�HD�@ D�� D��HD���D�>�D��HD�� D���D�AHD�~�D���D�  D�B�D��HD��HD�  D�@ D���D�D�  D�.D�o\>���?#�
?aG�?���?��@   @��@0��@G�@^�R@xQ�@��@�33@�  @��@�@\@�\)@�Q�@��@��@��HA�
A��A{A�
A=qA�RA%�A+�A1G�A6ffA<(�AA�AG
=AMp�AS33AXQ�A^�RAdz�Ai��Ap  AvffA{�A���A�(�A��RA���A�(�A��RA���A�33A�p�A�\)A��A��A��A�
=A���A�=qA�(�A�{A��A���A�33A���A�{A�Q�A���A�33A�p�A�
=A�Q�A�=qA�(�A�p�A�\)A�G�A\A�(�A�{AǮA���A��HA�z�A�AϮAљ�A��HA�(�A�{A׮A���Aڏ\A�z�A�p�A�
=A���A�\A�A�p�A�
=A�  A��A��
A���A�ffA�Q�A��A�33A���A��RA�  A���A��
A�p�A��RB Q�BG�B{B�HB�
B��BB�\B�B��B	B
�RB�
B��BB33B(�B�B{B�Bz�Bp�B�RB�
B��B{B33B  BG�BffB�B z�B!��B"�HB#�B$��B&{B'33B((�B)�B*�\B+�B,��B-B/
=B0(�B1�B2ffB3�B4��B5�B7
=B8Q�B9��B:�\B;�B=�B>{B?33B@z�BABB�HBC�
BE�BF�\BG�BH��BIBK33BLQ�BMG�BN�\BO�
BP��BQ�BS33BTz�BU��BV�\BW�
BY�BZ{B[
=B\Q�B]p�B^=qB_\)B`��Ba��BbffBc�Bd��Be�Bf�HBg�
Bi�Bj=qBk33BlQ�Bmp�Bn�\Bo�Bpz�Bq��Br�RBs�
Bt��Bu�Bv�HBw�
Bx��Bz{B{
=B{�
B|��B~{B33B�
B�Q�B���B�G�B���B��
B�(�B��\B��RB���B��HB�
=B�33B�33B��B�33B�\)B�p�B�\)B�p�B�p�B���B��B��B��B�B��B�  B��B�{B�=qB�Q�B�Q�B�ffB��\B���B��RB���B��HB���B��B�G�B�p�B�p�B��B���B�B��B�{B�=qB�=qB�Q�B�z�B���B���B���B��B��B�33B�\)B���B�B��B�  B�{B�=qB�ffB���B���B��HB���B��B�\)B���B�B��
B�  B�(�B�ffB���B���B���B��B�G�B�p�B��B��B�(�B�Q�B�ffB���B���B�
=B�\)B���B��
B�  B�(�B�ffB��\B��HB�33B�p�B��B��
B�{B�Q�B��\B���B�33B�p�B��B��B�{B�Q�B��\B���B�33B�p�B���B��
B�{B�Q�B���B���B�33B�p�B���B��B�(�B�ffB��RB�
=B�\)B���B��
B�{B�ffB���B�
=B�G�B��B�  B�Q�B��\B��HB�33B�p�B�B�{B�z�B��HB�G�B��B��B�=qB��\B���B�\)B�B�(�B��\B��HB�33B��B��
B�(�B��\B���B�\)B��B�  B�Q�B��RB���B�\)B�B�(�B��\B���B�\)B��B�  B�Q�B��RB��B�p�B��
B�Q�B��RB��B�p�B��
B�(�B�z�B��HB�33B���B��B�ffB���B�33B���B�  B�z�B��HB�G�B�B�{B��\B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B��B�(�B���B��B��B�  B�z�B���B�\)B��
B�Q�B���B�G�B��B�=qB��RB�33B��B�(�B£�B��BÅB�{Bď\B���BŅB��B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�Bʣ�B�33BˮB�(�Ḅ�B�33BͮB�=qBθRB�33BϮB�=qB���B�G�B��
B�Q�B��HB�\)B��B�z�B�
=Bՙ�B�(�BָRB�\)B��B�z�B��BٮB�=qB��HB�\)B�  B܏\B��BݮB�=qB��HB߅B�(�B�RB�\)B��B�ffB���B㙚B�=qB��HB�p�B�{B��B�33B�B�Q�B���B�B�{B�RB�G�B��B�z�B�
=B�B�(�B�RB�\)B��B��\B�33B�B�=qB���B�p�B�  B���B�G�B��
B�ffB���B�p�B�  B��\B�33B�B�ffB���B��B�{B��\B�33B�B�Q�B���B���C {C \)C ��C �C=qC�\C�HC(�Cp�CC
=CffC�RC
=CQ�C��C�C33C�C�HC33C�C��C{C\)C�C  CQ�C��C�HC	(�C	p�C	C
{C
ffC
�C
��C=qC�\C�C33Cz�CC
=C\)C�C  C=qC�C�HC(�Cz�C�RC  CG�C��C�C33Cz�CC
=C\)C�C�C33Cz�C��C�CffC�C  CQ�C��C��C33Cz�CC�CffC��C�C=qC�\C��C{C\)C�C��C33Cz�CC{CG�C�\C�C(�CffC�RC
=CG�C�C�HC(�Cp�C�C  CQ�C�\C�HC =qC z�C C!�C!\)C!��C"  C"\)C"��C"�C#G�C#�\C#�
C$(�C$z�C$��C%{C%ffC%�RC&  C&G�C&��C&�HC'{C'ffC'��C'�
C(  C(=qC(p�C(�\C(�C(�C)
=C)(�C)G�C)z�C)��C)C)�HC*�C*=qC*ffC*�C*�RC*�HC+  C+(�C+ffC+�C+��C+�HC,  C,�C,\)C,�C,��C,��C-
=C-33C-Q�C-�C-�RC-�HC.  C.=qC.ffC.�\C.C.��C/�C/G�C/�C/�C/�
C0
=C0=qC0ffC0��C0�
C1  C1(�C1\)C1�\C1�RC1�C2�C2\)C2�C2�C2�C3(�C3Q�C3�C3C3��C4�C4G�C4�\C4C4�C5�C5\)C5�\C5�RC5�C6(�C6\)C6�C6C7  C7(�C7\)C7��C7��C8  C8=qC8p�C8��C8�
C9{C9G�C9z�C9�RC9��C:�C:\)C:��C:��C;  C;=qC;p�C;��C;�HC<{C<G�C<�C<C<�C=(�C=ffC=�\C=��C>{C>=qC>p�C>�C>�C?�C?\)C?�\C?C@  C@=qC@p�C@��C@�CA(�CAQ�CA�\CA�
CB
=CB=qCB�CBCB��CC33CCp�CC�RCC��CD(�CD\)CD��CD�HCE�CEG�CEz�CE��CF
=CFG�CFz�CF�CF��CG33CGp�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          ?��@   @B�\@�G�@��\@\@޸R@��RA  A\)A*�HA@��A`  A~�RA�  A�Q�A�  A�  A�  A�  A�B   B  B(�B  B   B(  B0(�B8Q�B@(�BH(�BP  BX(�B`  Bh  Bp(�Bx(�B�=qB�{B��B��B��B��B�  B�  B�  B��B��B�{B�  B��B��B��B��B�  B�  B�  B�  B�  B�{B�  B��B��B��B�  B�{B�{B��B��C   C  C  C  C  C
  C
=C
=C
=C  C
=C
=C  C  C��C�C��C"  C$  C&  C({C*  C,  C-��C0  C2
=C4
=C5��C7��C:
=C<
=C>  C@  CB
=CD
=CF  CH  CJ  CL
=CN
=CP  CQ��CS��CV
=CX
=CZ
=C\  C^
=C_��Ca��Cd  Cf  Ch  Cj  Cl
=Cn
=Cp  Cq��Cs��Cv  Cx
=Cy��C{��C}��C�  C�C�C�  C���C���C�  C�C�C�C�  C�  C�C�  C���C�  C�C�C�  C�C�C�  C�  C�  C�  C�C���C���C�  C�  C�  C�C���C���C���C���C���C�  C�C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�C���C���C���C�  C�C�C���C���C�  C�  C���C�  C�C�C�
=C�C���C�  C�  C���C�  C�C�C�  C���C�  C���C�  C�
=C�C���C�  C�C�  C���C���C�  C���C���C�  C�  C���C�C�  C���C���C�  C�  C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�C���C�  C�C�  C�  C�C�C�C�C�  D   D � D  D}qD�qD}qD�qD}qD�qD}qD�qDz�D��Dz�D  D}qD�qD��D	  D	� D
  D
� D�D� D  D}qD�qD}qD�D��D��D}qD  D}qD�D�D�D� D  D��D�qD}qD�qD}qD�D��D  D� D�D}qD  D� D�D��D�qD� D�D� D�qD}qD  D�D�D� D�qD � D!�D!� D!�qD"� D#D#��D$  D$}qD$��D%z�D%��D&z�D&��D'z�D(  D(�D)�D)}qD)�qD*}qD+  D+� D+�qD,��D-�D-��D.  D.� D/  D/��D0�D0� D1  D1� D2  D2� D3  D3}qD4  D4� D5�D5��D5�qD6� D7D7}qD7�qD8��D9�D9� D9�qD:� D;�D;�D<�D<��D=  D=}qD=�qD>}qD?D?��D@�D@� DA  DA� DB  DB� DB��DC}qDD  DD� DE  DE}qDF  DF��DG  DG}qDG��DH}qDI  DI}qDJ  DJ�DK�DK� DK�qDL}qDL��DM� DN�DN� DO  DO}qDO�qDP}qDQ  DQ�DR�DR��DS�DS��DT�DT� DT�qDU� DV  DV}qDW  DW��DX  DX}qDY  DY��DZ�DZ}qDZ�qD[��D\�D\� D\��D]}qD^  D^� D_  D_� D`�D`��D`�qDa� Db�Db� Dc  Dc}qDc�qDdz�Dd�qDe��Df�Df� Dg  Dg}qDh  Dh� Dh�qDi}qDi��Dj}qDj�qDk}qDl  Dl� Dm�Dm�Dn�Dn� Do�Do��Do�qDp}qDp�qDq� Dr  Dr}qDs  Ds}qDs�qDt}qDu  Du� Dv  Dv}qDv�qDwz�Dw�qDx��Dy  Dy}qDz�Dz� Dz�qD{}qD{��D|}qD}  D}� D~  D~� D~�qD� D�  D�>�D�� D�� D���D�AHD��HD��HD�HD�@ D�~�D���D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D��HD��HD��D�AHD�� D���D���D�@ D��HD��HD�  D�AHD��HD���D�HD�@ D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�@ D�� D��HD�HD�>�D�� D�� D�  D�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D��qD���D�>�D�~�D�� D�  D�AHD�~�D���D���D�>�D�� D��HD�HD�@ D���D�D�HD�@ D�~�D��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�B�D��HD�� D�HD�AHD�~�D��qD��qD�=qD�~�D���D���D�@ D�� D��HD���D�@ D��HD���D���D�>�D�� D���D�HD�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�AHD�~�D��HD�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD���D��HD�  D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D�� D��HD�  D�>�D�}qD���D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�>�D�~�D���D�  D�B�D���D�D��D�@ D�}qD�� D�  D�=qD�� D�� D�  D�AHD�� D��qD�  D�>�D�� D�� D�HD�AHD���D�D�HD�@ D�~�D�� D�HD�AHD��HD�� D�HD�AHD D¾�D�  D�@ DÁHD�� D�HD�AHDāHD��HD�HD�@ Dŀ D��HD�HD�AHDƁHD��HD�  D�>�Dǀ D��HD�  D�>�D�~�D�� D�  D�@ D�~�Dɾ�D�  D�>�Dʀ D��HD�  D�@ DˁHD��HD�HD�AHD�~�D̽qD���D�AHD́HD��HD�  D�AHD΀ D�� D���D�>�Dπ D��HD�  D�@ DЀ D�� D�HD�AHDр D�� D�HD�@ DҁHD�� D���D�@ DӀ D�� D�HD�B�DԁHD�� D�HD�@ DՀ D�� D���D�>�DցHD��HD�  D�@ DׁHD�� D�HD�@ D؀ Dؾ�D�  D�AHDفHD��HD�  D�>�Dڀ D�� D�HD�@ D�}qD۾�D���D�AHD܀ D��HD�HD�>�D݀ D�� D�  D�AHDހ D޽qD���D�>�D߀ D�� D���D�AHD�� DྸD���D�>�D�~�DᾸD���D�>�D� D�� D�  D�@ D� D��HD��D�AHD䂏D��HD��D�AHD�~�D�� D�HD�@ D�~�D�qD�  D�AHD� D�� D��D�AHD�HD�� D�  D�@ D�HD��HD�  D�>�D�}qD꾸D�  D�@ D�HD��HD�  D�@ D�~�D�� D���D�@ D�HD��HD�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D�  D�@ D�HD�� D���D�=qD�~�D�D�HD�AHD�HD�� D�HD�AHD�HD��HD�HD�@ D�� D��HD���D�>�D��HD�� D���D�AHD�~�D���D�  D�B�D��HD��HD�  D�@ D���D�D�  D�.G�O�>���?#�
?aG�?���?��@   @��@0��@G�@^�R@xQ�@��@�33@�  @��@�@\@�\)@�Q�@��@��@��HA�
A��A{A�
A=qA�RA%�A+�A1G�A6ffA<(�AA�AG
=AMp�AS33AXQ�A^�RAdz�Ai��Ap  AvffA{�A���A�(�A��RA���A�(�A��RA���A�33A�p�A�\)A��A��A��A�
=A���A�=qA�(�A�{A��A���A�33A���A�{A�Q�A���A�33A�p�A�
=A�Q�A�=qA�(�A�p�A�\)A�G�A\A�(�A�{AǮA���A��HA�z�A�AϮAљ�A��HA�(�A�{A׮A���Aڏ\A�z�A�p�A�
=A���A�\A�A�p�A�
=A�  A��A��
A���A�ffA�Q�A��A�33A���A��RA�  A���A��
A�p�A��RB Q�BG�B{B�HB�
B��BB�\B�B��B	B
�RB�
B��BB33B(�B�B{B�Bz�Bp�B�RB�
B��B{B33B  BG�BffB�B z�B!��B"�HB#�B$��B&{B'33B((�B)�B*�\B+�B,��B-B/
=B0(�B1�B2ffB3�B4��B5�B7
=B8Q�B9��B:�\B;�B=�B>{B?33B@z�BABB�HBC�
BE�BF�\BG�BH��BIBK33BLQ�BMG�BN�\BO�
BP��BQ�BS33BTz�BU��BV�\BW�
BY�BZ{B[
=B\Q�B]p�B^=qB_\)B`��Ba��BbffBc�Bd��Be�Bf�HBg�
Bi�Bj=qBk33BlQ�Bmp�Bn�\Bo�Bpz�Bq��Br�RBs�
Bt��Bu�Bv�HBw�
Bx��Bz{B{
=B{�
B|��B~{B33B�
B�Q�B���B�G�B���B��
B�(�B��\B��RB���B��HB�
=B�33B�33B��B�33B�\)B�p�B�\)B�p�B�p�B���B��B��B��B�B��B�  B��B�{B�=qB�Q�B�Q�B�ffB��\B���B��RB���B��HB���B��B�G�B�p�B�p�B��B���B�B��B�{B�=qB�=qB�Q�B�z�B���B���B���B��B��B�33B�\)B���B�B��B�  B�{B�=qB�ffB���B���B��HB���B��B�\)B���B�B��
B�  B�(�B�ffB���B���B���B��B�G�B�p�B��B��B�(�B�Q�B�ffB���B���B�
=B�\)B���B��
B�  B�(�B�ffB��\B��HB�33B�p�B��B��
B�{B�Q�B��\B���B�33B�p�B��B��B�{B�Q�B��\B���B�33B�p�B���B��
B�{B�Q�B���B���B�33B�p�B���B��B�(�B�ffB��RB�
=B�\)B���B��
B�{B�ffB���B�
=B�G�B��B�  B�Q�B��\B��HB�33B�p�B�B�{B�z�B��HB�G�B��B��B�=qB��\B���B�\)B�B�(�B��\B��HB�33B��B��
B�(�B��\B���B�\)B��B�  B�Q�B��RB���B�\)B�B�(�B��\B���B�\)B��B�  B�Q�B��RB��B�p�B��
B�Q�B��RB��B�p�B��
B�(�B�z�B��HB�33B���B��B�ffB���B�33B���B�  B�z�B��HB�G�B�B�{B��\B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B��B�(�B���B��B��B�  B�z�B���B�\)B��
B�Q�B���B�G�B��B�=qB��RB�33B��B�(�B£�B��BÅB�{Bď\B���BŅB��B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�Bʣ�B�33BˮB�(�Ḅ�B�33BͮB�=qBθRB�33BϮB�=qB���B�G�B��
B�Q�B��HB�\)B��B�z�B�
=Bՙ�B�(�BָRB�\)B��B�z�B��BٮB�=qB��HB�\)B�  B܏\B��BݮB�=qB��HB߅B�(�B�RB�\)B��B�ffB���B㙚B�=qB��HB�p�B�{B��B�33B�B�Q�B���B�B�{B�RB�G�B��B�z�B�
=B�B�(�B�RB�\)B��B��\B�33B�B�=qB���B�p�B�  B���B�G�B��
B�ffB���B�p�B�  B��\B�33B�B�ffB���B��B�{B��\B�33B�B�Q�B���B���C {C \)C ��C �C=qC�\C�HC(�Cp�CC
=CffC�RC
=CQ�C��C�C33C�C�HC33C�C��C{C\)C�C  CQ�C��C�HC	(�C	p�C	C
{C
ffC
�C
��C=qC�\C�C33Cz�CC
=C\)C�C  C=qC�C�HC(�Cz�C�RC  CG�C��C�C33Cz�CC
=C\)C�C�C33Cz�C��C�CffC�C  CQ�C��C��C33Cz�CC�CffC��C�C=qC�\C��C{C\)C�C��C33Cz�CC{CG�C�\C�C(�CffC�RC
=CG�C�C�HC(�Cp�C�C  CQ�C�\C�HC =qC z�C C!�C!\)C!��C"  C"\)C"��C"�C#G�C#�\C#�
C$(�C$z�C$��C%{C%ffC%�RC&  C&G�C&��C&�HC'{C'ffC'��C'�
C(  C(=qC(p�C(�\C(�C(�C)
=C)(�C)G�C)z�C)��C)C)�HC*�C*=qC*ffC*�C*�RC*�HC+  C+(�C+ffC+�C+��C+�HC,  C,�C,\)C,�C,��C,��C-
=C-33C-Q�C-�C-�RC-�HC.  C.=qC.ffC.�\C.C.��C/�C/G�C/�C/�C/�
C0
=C0=qC0ffC0��C0�
C1  C1(�C1\)C1�\C1�RC1�C2�C2\)C2�C2�C2�C3(�C3Q�C3�C3C3��C4�C4G�C4�\C4C4�C5�C5\)C5�\C5�RC5�C6(�C6\)C6�C6C7  C7(�C7\)C7��C7��C8  C8=qC8p�C8��C8�
C9{C9G�C9z�C9�RC9��C:�C:\)C:��C:��C;  C;=qC;p�C;��C;�HC<{C<G�C<�C<C<�C=(�C=ffC=�\C=��C>{C>=qC>p�C>�C>�C?�C?\)C?�\C?C@  C@=qC@p�C@��C@�CA(�CAQ�CA�\CA�
CB
=CB=qCB�CBCB��CC33CCp�CC�RCC��CD(�CD\)CD��CD�HCE�CEG�CEz�CE��CF
=CFG�CFz�CF�CF��CG33CGp�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�9G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ƨA�ƨA���A���A���A�ĜA�ȴA�ȴA��
A���A�A�A�5?A��A��A���A�ȴA�A癚A���A��A�DA�x�A��A�!A߸RA�A�JA���A�VA� �Aٕ�A�C�A׾wAבhA� �AՉ7A�5?A�ȴA˼jA�9XA��A�{A�ĜA�$�A�S�A�?}A��A���A�dZA��A��7A�33A�
=A��A�{A�ZA�bNA��A��9A��A�oA�hsA�&�A�E�A��!A�5?A��PA{�TAx �Au|�Aq�hAmhsAj$�AgVAe��Ac�^A`I�A_VA^jA[��AW�;AS7LAO�AM��AJ�\AG�mAF��AD��AC%ABVAB(�AA\)A@ffA?��A>��A=��A=S�A=�A<�RA9�
A5��A5G�A4��A4�A3dZA2JA1x�A0ȴA0z�A0bA.E�A-��A-p�A,�A+�A)��A)G�A)VA)�A(��A((�A(�A'��A'��A&��A&��A&�A$��A#��A#�7A#;dA"�DA"1'A"�A!�PA!�A!l�A!\)A!G�A �A ^5A�#A�PAt�A"�A��A�+A(�A��A�A  AdZAȴA$�A�;Ax�AXA�RA��A7LA�!A��A7LA
=A�+A  A�wA��A�7Ap�A\)A��A-A�wAO�A��AM�A-A��AƨAl�A7LA��A�+AM�AbA��AC�A��AĜA�+A$�Al�A%A
�yA
�jA
v�A
1'A	�A	�-A	��A	hsA	?}A	"�A	A�uA9XA��A�;A�FAp�A?}A�yA�\AM�AA�;A�^A��AO�A��A=qA�
A|�AXA&�Az�A$�A��A�;AA��Ax�AO�A ��A ��A �DA 9X@���@��@��\@�-@��@��@��@�1@��@��h@���@���@���@�5?@�x�@�@�w@�@�+@�V@�@�h@�O�@��@�@�9X@@��@�J@�V@��@�C�@���@��#@�@�O�@�j@��@睲@�t�@�"�@�
=@��y@���@�ff@�M�@�{@��T@�G�@�z�@�Z@��@��@�E�@�h@���@�Z@�Q�@��@�K�@ޗ�@��T@��/@� �@�l�@�=q@ش9@�|�@�o@�ff@��@���@��@�9X@���@�33@��H@���@ҟ�@�ff@�E�@���@�hs@���@��@Η�@�{@Ͳ-@�`B@���@̃@�(�@��
@�+@���@�M�@�-@�@ȓu@� �@��@���@�|�@�o@���@���@�n�@�{@��@Ĭ@�Z@�b@î@�@�5?@�`B@��u@� �@�  @��;@�S�@�
=@���@�V@��#@�7L@��j@�A�@�K�@���@���@���@�X@�Ĝ@��@�1'@���@��F@�dZ@���@�ff@�7L@��@��/@���@��j@��D@�bN@�Z@�Q�@�(�@��;@��w@��@���@�+@�V@��@��T@�@�G�@��@���@���@���@��y@��\@�5?@�{@���@��^@��@��@���@�bN@�ƨ@�ȴ@���@�~�@�=q@��T@���@�x�@�O�@���@��w@�|�@�;d@��R@��+@�n�@��@��@�r�@�ƨ@���@��P@��P@��@�t�@�l�@�S�@���@��R@�v�@�M�@��T@�`B@��9@��;@�33@�o@��y@���@�-@��@���@�X@�?}@�/@���@��@���@���@��D@�r�@�I�@� �@��w@�S�@�33@��y@���@�ff@��@��T@�@���@��h@�hs@��@��@���@�Ĝ@��9@��u@�z�@�b@��
@���@�t�@�o@���@�E�@��-@��@�?}@�&�@�V@���@��@��`@��/@�Ĝ@�z�@�9X@�  @��w@�+@��y@��R@���@��+@�n�@�E�@�{@��@��@�O�@�&�@���@���@�Q�@���@��w@��@�t�@�C�@���@���@�J@��-@��7@�hs@�?}@�/@�%@��j@��@�Q�@��w@�dZ@�33@��y@��!@��\@�v�@�^5@��T@��7@�&�@���@��`@���@���@�z�@�I�@�b@�  @���@��@��;@��w@���@�|�@���@��#@��7@�x�@�O�@�?}@��@��`@��@���@�A�@�b@���@��m@���@��@�V@��@�@���@�X@���@��@���@���@��@�b@��@��@|�@
=@~��@~$�@}`B@|��@|j@{t�@z�H@z~�@zM�@zJ@yx�@x�@xb@w��@w�@w
=@v{@u��@up�@t��@tz�@t�@st�@r��@q�@q��@q&�@pr�@o��@o�P@oK�@o�@n�+@nE�@n{@mp�@l��@lZ@l�@k��@k��@kt�@j�!@j~�@i��@i�@i��@h��@hb@g��@gl�@f�@e��@e�@e?}@d��@d�D@d(�@c�
@c��@c@b^5@a�7@a�@` �@^�y@^E�@]��@\��@\�j@\z�@\I�@[�F@Z��@Z=q@Y��@Y&�@X�u@X �@X  @W�w@W��@W�P@W�P@W|�@W\)@V�y@Vff@U��@U?}@T��@T1@S��@SS�@R��@R�@Q��@Q��@Q��@Q7L@Q%@P�u@PQ�@PA�@O�@O
=@N��@N�R@M��@L�/@LI�@Kt�@J��@JJ@I�^@I��@Ix�@Ihs@H�`@H��@H�@H1'@F�y@E��@E��@Ep�@D�@Dj@D�@C��@C33@C@B�H@B��@B��@B�!@A��@A�7@A7L@@�9@@��@@�@@r�@?�;@?l�@?;d@>�y@>v�@>E�@=�T@=`B@=�@<��@<j@<(�@;ƨ@;S�@;33@:��@:�\@:�\@:n�@9��@9&�@9�@8�`@8�u@8  @7�P@7|�@7l�@7l�@7;d@6�@6@5��@5O�@5/@5V@4�/@4�/@4�j@4j@3��@3��@3S�@3@2�@2�!@2~�@2^5@2M�@2-@1�7@1�7@1hs@1�@1�@0Ĝ@/�;@/l�@/+@/�@/�@/
=@/
=@.��@.�y@.�@.�R@.��@.E�@-�T@-�-@-?}@,��@,�j@,��@,Z@,�@+��@+S�@+"�@+@*��@*��@*��@*��@*^5@*�@)�^@)X@)&�@(�u@(b@'�;@'�@'l�@'+@&�+@&$�@&{@%�@%�h@%V@$��@$z�@$�@#�m@#�
@#�F@#dZ@#"�@#o@"�@"��@"��@"M�@"-@"�@"J@!�@!�@ ��@ r�@ 1'@�;@��@\)@�@�R@��@�+@V@E�@$�@�T@�@`B@O�@�@��@�D@9X@�m@�F@��@�@dZ@"�@�@�H@��@~�@~�@^5@J@�7@�7@hs@G�@X@hs@hs@&�@�`@��@bN@  @�@��@�@\)@;d@�@ȴ@�+@�+@v�@V@$�@{@{@�@�T@��@��@�h@�@`B@O�@/@��@��@��@��@��@�D@j@(�@1@��@��@��@�F@dZ@C�@"�@"�@o@��@��@��@M�@J@�#@�^@��@x�@x�@G�@��@�u@bN@1'@�@�w@�P@|�@|�@+@�y@ȴ@��@��@�+@�+@�+@v�@ff@ff@5?@$�@@�@��@��@�@`B@��@�@��@�@�/@�/@�j@�@Z@(�@�@�
@�
@ƨ@�F@��@��@�@S�@S�@C�@C�A�ĜA�ȴA�ȴA���A���A���A���A�ƨA�A�ȴA�ƨA�A�ƨA���A���A���A�ƨA�^A�jA�ƨA�A�ƨA�ƨA�ƨA�ƨA���A�ȴA�A���A��A���A���A�ĜA���A�ƨA���A�A�-A�9A�A�\A�A�7A藍A蕁A�r�A�A�A�=qA�K�A�;dA�(�A�(�A�"�A�&�A�"�A��A��A��A�1A��A�bA�
=A�
=A�A���A��A���A���A��A��A��mA��HA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ĜA�ĜA���A�wA�ĜA�A�wA���A�ĜA���A���A�ĜA���A���A�jA��A��A��A盦A畁A�A�|�A�~�A�jA�E�A�-A��A�A��`A�~�A�M�A�A�A�=qA�&�A��TA�l�A�VA�p�A�/A��A��#A�ƨA�ƨA�ĜA㛦A�x�A�C�A�-A� �A�A�+A�K�A�A�A�7LA�(�A��A�oA�1A���A��A��A��#A�-A�K�A���A�wA�|�A�`BA�M�A�G�A�JA��A���AߍPA߃AߍPAߋDA߃A�x�A�hsA�E�A�ȴA�bNA��AݸRA�XA�1A��#A���AܮA܁A�`BA�C�A�1A���A�A۴9AۍPA�hsA�M�A�bA��yA�ƨAڡ�A�r�A�O�A�A�A�"�A�
=A���A��TA���A�ĜAټjAٸRA٩�AكA�Q�A�JAأ�A�I�A�C�A�;dA�1'A��A�1A��`A�ȴA�ĜA׾wA׸RA״9A׶FAײ-Aס�Aו�AדuAם�Aו�A׋DA�x�A�r�A׃A׋DA�ZA�+A�VA���A��`A���AָRAִ9Aֲ-A֝�Aև+AփAցA�z�A�r�A�jA�hsA�`BA�VA�I�A�?}A�9XA�1'A��A�  A���A��A��yA��#A���A�ĜAհ!AՓuA�x�A�ffA�Q�A�9XA�{A��yA���Aԛ�A�r�A�-A��A�Aӧ�AӑhA�~�A�hsA�K�A�33A�$�A��A�
=A��A��
A�Aҡ�A҅A�z�A�p�A�hsA�VA�;dA��A���A��mA���A�ĜAѸRAѩ�Aћ�AуA�^5A�ffA�ZA�G�A�+A�{A���A��#AиRAЙ�A�9XAϩ�Aϣ�AϓuAϕ�Aϕ�Aχ+A�|�A�ffA�bNA�M�A��A��A�x�A�oA��A͕�A�p�A�G�A��
A̝�A�M�A���AˋDA�?}A��;A�z�A��yA�bNA�(�A��`Aȧ�AȁA�hsA�ZA�9XA�-A�-A�&�A�$�A�"�A�"�A��A��A�JA�%A��A�K�Aƴ9A�K�A� �A�VA�1A��A�XA�jAüjA��A���A�A�t�A�Q�A�(�A���A���A��7A�S�A�A��PA�E�A��A���A��A��jA��A�ZA��`A���A�|�A�l�A�\)A�K�A�/A��A��`A���A���A��A�E�A�oA��A���A��!A�bNA�oA��#A��A�E�A��A���A���A�M�A��-A��TA�ZA�5?A��A���A��uA��DA��+A�z�A�l�A�=qA��;A�A�A�v�A��A��A���A�\)A��yA��+A�C�A�A��9A�z�A�E�A�{A��wA�|�A�ffA�M�A�33A�&�A�oA�A��TA��A��A�Q�A��A���A��`A��/A���A�ĜA���A�hsA�oA���A��+A��;A��A�$�A�r�A�JA�ȴA�z�A�  A�~�A���A�;dA��A��wA�G�A�%A��wA�\)A�7LA��`A��9A�n�A��/A��A��A�&�A��TA���A�\)A���A�ĜA�Q�A��A��!A���A�G�A���A��DA���A��hA�r�A�ZA�
=A���A���A�O�A���A��^A��A�S�A��A���A�t�A�$�A���A�$�A�;dA�ĜA��\A�XA�/A�
=A�  A��`A�ȴA��wA�A��jA��9A��-A���A���A���A��uA��PA�~�A�n�A�dZA�VA�K�A�7LA�-A�bA��A��FA�dZA��A��/A�p�A��A�ȴA���A��A�l�A�VA�A�A� �A��A��9A�^5A��`A�  A���A�^5A���A���A�|�A�ZA� �A�ȴA�Q�A�ƨA�hsA�-A���A���A�7LA���A�M�A��A��PA�oA���A�mA~�/A~bNA~  A}XA|�A|Q�A{�FA{
=AzJAy33Ax�Ax$�Ax�Aw�mAw��AwC�Av�Av��Au�At�yAtv�AtAs|�ArȴArJAq;dAp��Ap��Ap=qAoAn�HAm�TAl��AlI�Ak�AkVAkAj�Aj��AjZAit�Ah�AhJAg�FAghsAgC�AfĜAfffAfE�Af9XAf(�Ae�wAehsAe`BAe+Ae%Ad��Ad��Adr�Ad�Ac|�Ab1'Aax�AaC�A`��A`I�A`�A_�A_�^A_�PA_hsA_"�A_%A^�A^�`A^�/A^��A^ĜA^�9A^��A^n�A^1'A]�^A]|�A]O�A]A\ĜA[�;AZ�AY�
AYC�AX�AX�!AX^5AWx�AW�AV�AU/AT��ATbNAS�ARĜAQ�^AP�AP��APZAP�AO�mAO��AO��AO�AO;dAN�`ANE�AM�AM&�AL�!ALM�AL�AK�^AK�AJ{AI�AH�`AHjAH9XAH�AG�TAG�FAG�hAGp�AGO�AG7LAG"�AG
=AF�HAF�jAF�uAF�AFv�AFZAF(�AE�mAE��AEp�AE?}AE�AD��AD�AD�AD��ADbNADA�AD9XAD �ADAC��AC��AC&�AB�ABĜAB��AB�+AB~�ABv�ABjABffABffAB^5ABZABZABZABM�ABQ�ABQ�ABM�ABM�ABM�ABQ�ABI�ABE�ABI�ABA�AB5?AB(�AB{AA�AA�AA�mAA�wAA��AA��AA�AAdZAAK�AA;dAA"�AAVA@��A@�A@ȴA@��A@�A@ffA@M�A@5?A@$�A@�A@1A?��A?�A?�mA?�
A?ƨA?�wA?�FA?��A?�hA?�A?t�A?S�A?&�A?A>��A>��A>z�A>I�A> �A>1A=�A=��A=�-A=��A=��A=�PA=�7A=|�A=l�A=hsA=hsA=`BA=XA=XA=K�A=G�A=C�A=7LA=/A=+A=&�A=�A=�A=�A=oA=A<��A<��A<�A<�A<�`A<��A<�9A<��A<n�A<Q�A<bA;��A;C�A:Q�A:1A9x�A8��A7�A6�/A6-A5�A5�PA5l�A5dZA5`BA5XA5XA5XA5XA5S�A5O�A5K�A5G�A5?}A533A5+A5+A5"�A5�A5VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          A���A���A�ƨA�ƨA���A���A���A�ĜA�ȴA�ȴA��
A���A�A�A�5?A��A��A���A�ȴA�A癚A���A��A�DA�x�A��A�!A߸RA�A�JA���A�VA� �Aٕ�A�C�A׾wAבhA� �AՉ7A�5?A�ȴA˼jA�9XA��A�{A�ĜA�$�A�S�A�?}A��A���A�dZA��A��7A�33A�
=A��A�{A�ZA�bNA��A��9A��A�oA�hsA�&�A�E�A��!A�5?A��PA{�TAx �Au|�Aq�hAmhsAj$�AgVAe��Ac�^A`I�A_VA^jA[��AW�;AS7LAO�AM��AJ�\AG�mAF��AD��AC%ABVAB(�AA\)A@ffA?��A>��A=��A=S�A=�A<�RA9�
A5��A5G�A4��A4�A3dZA2JA1x�A0ȴA0z�A0bA.E�A-��A-p�A,�A+�A)��A)G�A)VA)�A(��A((�A(�A'��A'��A&��A&��A&�A$��A#��A#�7A#;dA"�DA"1'A"�A!�PA!�A!l�A!\)A!G�A �A ^5A�#A�PAt�A"�A��A�+A(�A��A�A  AdZAȴA$�A�;Ax�AXA�RA��A7LA�!A��A7LA
=A�+A  A�wA��A�7Ap�A\)A��A-A�wAO�A��AM�A-A��AƨAl�A7LA��A�+AM�AbA��AC�A��AĜA�+A$�Al�A%A
�yA
�jA
v�A
1'A	�A	�-A	��A	hsA	?}A	"�A	A�uA9XA��A�;A�FAp�A?}A�yA�\AM�AA�;A�^A��AO�A��A=qA�
A|�AXA&�Az�A$�A��A�;AA��Ax�AO�A ��A ��A �DA 9X@���@��@��\@�-@��@��@��@�1@��@��h@���@���@���@�5?@�x�@�@�w@�@�+@�V@�@�h@�O�@��@�@�9X@@��@�J@�V@��@�C�@���@��#@�@�O�@�j@��@睲@�t�@�"�@�
=@��y@���@�ff@�M�@�{@��T@�G�@�z�@�Z@��@��@�E�@�h@���@�Z@�Q�@��@�K�@ޗ�@��T@��/@� �@�l�@�=q@ش9@�|�@�o@�ff@��@���@��@�9X@���@�33@��H@���@ҟ�@�ff@�E�@���@�hs@���@��@Η�@�{@Ͳ-@�`B@���@̃@�(�@��
@�+@���@�M�@�-@�@ȓu@� �@��@���@�|�@�o@���@���@�n�@�{@��@Ĭ@�Z@�b@î@�@�5?@�`B@��u@� �@�  @��;@�S�@�
=@���@�V@��#@�7L@��j@�A�@�K�@���@���@���@�X@�Ĝ@��@�1'@���@��F@�dZ@���@�ff@�7L@��@��/@���@��j@��D@�bN@�Z@�Q�@�(�@��;@��w@��@���@�+@�V@��@��T@�@�G�@��@���@���@���@��y@��\@�5?@�{@���@��^@��@��@���@�bN@�ƨ@�ȴ@���@�~�@�=q@��T@���@�x�@�O�@���@��w@�|�@�;d@��R@��+@�n�@��@��@�r�@�ƨ@���@��P@��P@��@�t�@�l�@�S�@���@��R@�v�@�M�@��T@�`B@��9@��;@�33@�o@��y@���@�-@��@���@�X@�?}@�/@���@��@���@���@��D@�r�@�I�@� �@��w@�S�@�33@��y@���@�ff@��@��T@�@���@��h@�hs@��@��@���@�Ĝ@��9@��u@�z�@�b@��
@���@�t�@�o@���@�E�@��-@��@�?}@�&�@�V@���@��@��`@��/@�Ĝ@�z�@�9X@�  @��w@�+@��y@��R@���@��+@�n�@�E�@�{@��@��@�O�@�&�@���@���@�Q�@���@��w@��@�t�@�C�@���@���@�J@��-@��7@�hs@�?}@�/@�%@��j@��@�Q�@��w@�dZ@�33@��y@��!@��\@�v�@�^5@��T@��7@�&�@���@��`@���@���@�z�@�I�@�b@�  @���@��@��;@��w@���@�|�@���@��#@��7@�x�@�O�@�?}@��@��`@��@���@�A�@�b@���@��m@���@��@�V@��@�@���@�X@���@��@���@���@��@�b@��@��@|�@
=@~��@~$�@}`B@|��@|j@{t�@z�H@z~�@zM�@zJ@yx�@x�@xb@w��@w�@w
=@v{@u��@up�@t��@tz�@t�@st�@r��@q�@q��@q&�@pr�@o��@o�P@oK�@o�@n�+@nE�@n{@mp�@l��@lZ@l�@k��@k��@kt�@j�!@j~�@i��@i�@i��@h��@hb@g��@gl�@f�@e��@e�@e?}@d��@d�D@d(�@c�
@c��@c@b^5@a�7@a�@` �@^�y@^E�@]��@\��@\�j@\z�@\I�@[�F@Z��@Z=q@Y��@Y&�@X�u@X �@X  @W�w@W��@W�P@W�P@W|�@W\)@V�y@Vff@U��@U?}@T��@T1@S��@SS�@R��@R�@Q��@Q��@Q��@Q7L@Q%@P�u@PQ�@PA�@O�@O
=@N��@N�R@M��@L�/@LI�@Kt�@J��@JJ@I�^@I��@Ix�@Ihs@H�`@H��@H�@H1'@F�y@E��@E��@Ep�@D�@Dj@D�@C��@C33@C@B�H@B��@B��@B�!@A��@A�7@A7L@@�9@@��@@�@@r�@?�;@?l�@?;d@>�y@>v�@>E�@=�T@=`B@=�@<��@<j@<(�@;ƨ@;S�@;33@:��@:�\@:�\@:n�@9��@9&�@9�@8�`@8�u@8  @7�P@7|�@7l�@7l�@7;d@6�@6@5��@5O�@5/@5V@4�/@4�/@4�j@4j@3��@3��@3S�@3@2�@2�!@2~�@2^5@2M�@2-@1�7@1�7@1hs@1�@1�@0Ĝ@/�;@/l�@/+@/�@/�@/
=@/
=@.��@.�y@.�@.�R@.��@.E�@-�T@-�-@-?}@,��@,�j@,��@,Z@,�@+��@+S�@+"�@+@*��@*��@*��@*��@*^5@*�@)�^@)X@)&�@(�u@(b@'�;@'�@'l�@'+@&�+@&$�@&{@%�@%�h@%V@$��@$z�@$�@#�m@#�
@#�F@#dZ@#"�@#o@"�@"��@"��@"M�@"-@"�@"J@!�@!�@ ��@ r�@ 1'@�;@��@\)@�@�R@��@�+@V@E�@$�@�T@�@`B@O�@�@��@�D@9X@�m@�F@��@�@dZ@"�@�@�H@��@~�@~�@^5@J@�7@�7@hs@G�@X@hs@hs@&�@�`@��@bN@  @�@��@�@\)@;d@�@ȴ@�+@�+@v�@V@$�@{@{@�@�T@��@��@�h@�@`B@O�@/@��@��@��@��@��@�D@j@(�@1@��@��@��@�F@dZ@C�@"�@"�@o@��@��@��@M�@J@�#@�^@��@x�@x�@G�@��@�u@bN@1'@�@�w@�P@|�@|�@+@�y@ȴ@��@��@�+@�+@�+@v�@ff@ff@5?@$�@@�@��@��@�@`B@��@�@��@�@�/@�/@�j@�@Z@(�@�@�
@�
@ƨ@�F@��@��@�@S�@S�@C�G�O�A�ĜA�ȴA�ȴA���A���A���A���A�ƨA�A�ȴA�ƨA�A�ƨA���A���A���A�ƨA�^A�jA�ƨA�A�ƨA�ƨA�ƨA�ƨA���A�ȴA�A���A��A���A���A�ĜA���A�ƨA���A�A�-A�9A�A�\A�A�7A藍A蕁A�r�A�A�A�=qA�K�A�;dA�(�A�(�A�"�A�&�A�"�A��A��A��A�1A��A�bA�
=A�
=A�A���A��A���A���A��A��A��mA��HA��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ĜA�ĜA���A�wA�ĜA�A�wA���A�ĜA���A���A�ĜA���A���A�jA��A��A��A盦A畁A�A�|�A�~�A�jA�E�A�-A��A�A��`A�~�A�M�A�A�A�=qA�&�A��TA�l�A�VA�p�A�/A��A��#A�ƨA�ƨA�ĜA㛦A�x�A�C�A�-A� �A�A�+A�K�A�A�A�7LA�(�A��A�oA�1A���A��A��A��#A�-A�K�A���A�wA�|�A�`BA�M�A�G�A�JA��A���AߍPA߃AߍPAߋDA߃A�x�A�hsA�E�A�ȴA�bNA��AݸRA�XA�1A��#A���AܮA܁A�`BA�C�A�1A���A�A۴9AۍPA�hsA�M�A�bA��yA�ƨAڡ�A�r�A�O�A�A�A�"�A�
=A���A��TA���A�ĜAټjAٸRA٩�AكA�Q�A�JAأ�A�I�A�C�A�;dA�1'A��A�1A��`A�ȴA�ĜA׾wA׸RA״9A׶FAײ-Aס�Aו�AדuAם�Aו�A׋DA�x�A�r�A׃A׋DA�ZA�+A�VA���A��`A���AָRAִ9Aֲ-A֝�Aև+AփAցA�z�A�r�A�jA�hsA�`BA�VA�I�A�?}A�9XA�1'A��A�  A���A��A��yA��#A���A�ĜAհ!AՓuA�x�A�ffA�Q�A�9XA�{A��yA���Aԛ�A�r�A�-A��A�Aӧ�AӑhA�~�A�hsA�K�A�33A�$�A��A�
=A��A��
A�Aҡ�A҅A�z�A�p�A�hsA�VA�;dA��A���A��mA���A�ĜAѸRAѩ�Aћ�AуA�^5A�ffA�ZA�G�A�+A�{A���A��#AиRAЙ�A�9XAϩ�Aϣ�AϓuAϕ�Aϕ�Aχ+A�|�A�ffA�bNA�M�A��A��A�x�A�oA��A͕�A�p�A�G�A��
A̝�A�M�A���AˋDA�?}A��;A�z�A��yA�bNA�(�A��`Aȧ�AȁA�hsA�ZA�9XA�-A�-A�&�A�$�A�"�A�"�A��A��A�JA�%A��A�K�Aƴ9A�K�A� �A�VA�1A��A�XA�jAüjA��A���A�A�t�A�Q�A�(�A���A���A��7A�S�A�A��PA�E�A��A���A��A��jA��A�ZA��`A���A�|�A�l�A�\)A�K�A�/A��A��`A���A���A��A�E�A�oA��A���A��!A�bNA�oA��#A��A�E�A��A���A���A�M�A��-A��TA�ZA�5?A��A���A��uA��DA��+A�z�A�l�A�=qA��;A�A�A�v�A��A��A���A�\)A��yA��+A�C�A�A��9A�z�A�E�A�{A��wA�|�A�ffA�M�A�33A�&�A�oA�A��TA��A��A�Q�A��A���A��`A��/A���A�ĜA���A�hsA�oA���A��+A��;A��A�$�A�r�A�JA�ȴA�z�A�  A�~�A���A�;dA��A��wA�G�A�%A��wA�\)A�7LA��`A��9A�n�A��/A��A��A�&�A��TA���A�\)A���A�ĜA�Q�A��A��!A���A�G�A���A��DA���A��hA�r�A�ZA�
=A���A���A�O�A���A��^A��A�S�A��A���A�t�A�$�A���A�$�A�;dA�ĜA��\A�XA�/A�
=A�  A��`A�ȴA��wA�A��jA��9A��-A���A���A���A��uA��PA�~�A�n�A�dZA�VA�K�A�7LA�-A�bA��A��FA�dZA��A��/A�p�A��A�ȴA���A��A�l�A�VA�A�A� �A��A��9A�^5A��`A�  A���A�^5A���A���A�|�A�ZA� �A�ȴA�Q�A�ƨA�hsA�-A���A���A�7LA���A�M�A��A��PA�oA���A�mA~�/A~bNA~  A}XA|�A|Q�A{�FA{
=AzJAy33Ax�Ax$�Ax�Aw�mAw��AwC�Av�Av��Au�At�yAtv�AtAs|�ArȴArJAq;dAp��Ap��Ap=qAoAn�HAm�TAl��AlI�Ak�AkVAkAj�Aj��AjZAit�Ah�AhJAg�FAghsAgC�AfĜAfffAfE�Af9XAf(�Ae�wAehsAe`BAe+Ae%Ad��Ad��Adr�Ad�Ac|�Ab1'Aax�AaC�A`��A`I�A`�A_�A_�^A_�PA_hsA_"�A_%A^�A^�`A^�/A^��A^ĜA^�9A^��A^n�A^1'A]�^A]|�A]O�A]A\ĜA[�;AZ�AY�
AYC�AX�AX�!AX^5AWx�AW�AV�AU/AT��ATbNAS�ARĜAQ�^AP�AP��APZAP�AO�mAO��AO��AO�AO;dAN�`ANE�AM�AM&�AL�!ALM�AL�AK�^AK�AJ{AI�AH�`AHjAH9XAH�AG�TAG�FAG�hAGp�AGO�AG7LAG"�AG
=AF�HAF�jAF�uAF�AFv�AFZAF(�AE�mAE��AEp�AE?}AE�AD��AD�AD�AD��ADbNADA�AD9XAD �ADAC��AC��AC&�AB�ABĜAB��AB�+AB~�ABv�ABjABffABffAB^5ABZABZABZABM�ABQ�ABQ�ABM�ABM�ABM�ABQ�ABI�ABE�ABI�ABA�AB5?AB(�AB{AA�AA�AA�mAA�wAA��AA��AA�AAdZAAK�AA;dAA"�AAVA@��A@�A@ȴA@��A@�A@ffA@M�A@5?A@$�A@�A@1A?��A?�A?�mA?�
A?ƨA?�wA?�FA?��A?�hA?�A?t�A?S�A?&�A?A>��A>��A>z�A>I�A> �A>1A=�A=��A=�-A=��A=��A=�PA=�7A=|�A=l�A=hsA=hsA=`BA=XA=XA=K�A=G�A=C�A=7LA=/A=+A=&�A=�A=�A=�A=oA=A<��A<��A<�A<�A<�`A<��A<�9A<��A<n�A<Q�A<bA;��A;C�A:Q�A:1A9x�A8��A7�A6�/A6-A5�A5�PA5l�A5dZA5`BA5XA5XA5XA5XA5S�A5O�A5K�A5G�A5?}A533A5+A5+A5"�A5�A5VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�'B�[B�[B�[B��B��B��B��B�UB��B�OB�!B�B�6B�B�:B��B��B��B��B��B��B	V�B	��B
 4B
SB
_B
�B
B	��B	�B	��B	�`B	�B	��B
7B
%FB
�B
��B
y�B
tTB
pB
_;B
{B
�?B.BI�Bb�B_B]/B\]BQ�BFB
��B
��B
�1B
XEB
4�B
*eB
$@B	�(B	�B	��B	�KB	�2B	�,B	��B	��B	��B	��B	y�B	f2B	\]B	N�B	EmB	9�B	5�B	/�B	3�B	0UB	-B	+�B	1'B	/B	0UB	&�B	)�B	,�B	0�B	2aB	9�B	8RB	8�B	<jB	A�B	F�B	J�B	T�B	aB	d�B	e�B	f2B	p�B	m]B	p�B	w�B	� B	��B	y	B	w�B	u�B	x8B	��B	��B	��B	�7B	��B	��B	��B	�zB	��B	�XB	�OB	�B	�EB	�KB	�jB	��B	��B	��B	�B	�B	��B	��B
MB
fB
�B
hB
:B
B
�B
�B
�B
 'B
#�B
$�B
$�B
&�B
'�B
(�B
)�B
,qB
/�B
0!B
4B
5�B
4�B
4�B
4B
33B
0UB
/�B
/�B
/�B
1�B
2�B
2�B
5B
7�B
7�B
7�B
7B
6zB
6FB
8RB
9XB
9�B
9�B
:�B
9�B
9XB
9�B
9�B
:�B
;�B
<jB
;�B
>B
B'B
A�B
?�B
?HB
=qB
<�B
:�B
;�B
9�B
9�B
9�B
:^B
:*B
:�B
:�B
;dB
;dB
;dB
;�B
;�B
=qB
=B
=�B
=�B
?�B
@�B
@B
?}B
@B
?�B
@�B
@�B
@�B
@B
@B
:�B
9XB
6FB
6zB
5B
5�B
5B
3�B
33B
2�B
2�B
1�B
0�B
0�B
0UB
/B
.�B
.B
-CB
-CB
+�B
+B
+B
)�B
)_B
($B
&�B
%�B
#B
#�B
!�B
 �B
 \B
�B
B
�B
�B
CB
xB
�B
	B
=B
=B
�B
�B
B
�B
eB
1B
1B
eB
�B
YB
_B
B
�B
�B
+B
�B
�B
�B
YB
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
B
�B
uB
�B
oB
�B
4B
4B
�B
\B
�B
(B
�B
VB
B
�B
JB
JB
~B
~B
�B
JB
B
B
�B
B
�B
�B
B
�B
xB
B
�B
�B
xB
�B
DB
DB
DB
xB
B
�B
B
�B
�B
~B
B
B
~B
B
B
�B
JB
~B
B
�B
JB
JB
B
B
B
�B
�B
~B
B
�B
~B
�B
JB
xB
�B
�B
B
�B
�B
"B
(B
�B
�B
�B
�B
4B
�B
.B
�B
bB
:B
B
�B
�B
hB
�B
B
�B
�B
hB
�B
�B
4B
�B
 B
oB
�B
:B
�B
�B
B
�B
B
�B
�B
�B
�B
uB
�B
uB
B
�B
B
�B
�B
�B
$B
B
B
SB
$B
�B
�B
SB
+B
+B
�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
�B
�B
7B
�B
�B
�B
eB
eB
�B
�B
�B
1B
1B
�B
�B
B
kB
�B
�B
B
�B
�B
�B
�B
B
B
�B
�B
VB
VB
VB
�B
�B
�B
 �B
!-B
!bB
!-B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#:B
#�B
$@B
$B
$@B
$tB
$�B
%B
&LB
&�B
&�B
'B
&�B
'RB
'RB
'RB
'B
'B
'B
'�B
'�B
'�B
($B
(�B
(�B
(�B
)*B
)*B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
)�B
*eB
*0B
)�B
*0B
*eB
*eB
*�B
*�B
*eB
*�B
+6B
+kB
+6B
+6B
+kB
*�B
+B
+B
+B
+B
+kB
+kB
+kB
+�B
+�B
,=B
,qB
,qB
,�B
,�B
,�B
-B
-B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
-wB
-B
/�B
/�B
/�B
/�B
0UB
0UB
0�B
0�B
0�B
0�B
1�B
1�B
1[B
1'B
1�B
2�B
49B
49B
49B
4B
4�B
5B
5�B
5tB
5B
5�B
6�B
6�B
7B
6�B
7�B
7LB
8B
8RB
8�B
8�B
9�B
:^B
9�B
:*B
:*B
:�B
;0B
;0B
:�B
:�B
;dB
;�B
;0B
;�B
<6B
<B
<6B
<�B
=�B
=�B
=�B
>B
>�B
?B
?B
?B
?HB
?�B
?�B
?}B
@�B
@�B
AUB
A B
A B
@�B
A B
@�B
@�B
AUB
A�B
A�B
C-B
B�B
B�B
B'B
B'B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
DgB
D�B
E�B
E�B
F�B
GzB
GEB
G�B
HKB
HKB
HKB
HB
H�B
IB
I�B
I�B
J�B
J�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LdB
L�B
MB
MB
M�B
M�B
NB
N�B
OB
OB
N�B
OB
O�B
O�B
PB
O�B
O�B
PHB
P�B
P�B
P�B
Q�B
Q�B
R B
RTB
R�B
R�B
R�B
R�B
S&B
R�B
S&B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U�B
VB
VB
U�B
W
B
V�B
V�B
VmB
VmB
V9B
W
B
W?B
W�B
XB
XyB
XyB
XEB
YKB
YKB
YB
YB
ZB
Y�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
\)B
\�B
\�B
\�B
]/B
\�B
\�B
^B
]�B
^B
^5B
^jB
_�B
_;B
_�B
_�B
_pB
_pB
`BB
aB
a|B
a�B
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
c B
b�B
b�B
c B
c�B
c�B
c�B
c�B
d&B
c�B
d&B
d�B
d�B
e,B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
e�B
e�B
f2B
ffB
gB
g8B
g8B
g�B
g�B
h
B
g�B
h
B
h>B
h�B
iB
iDB
iDB
iyB
iyB
iyB
iyB
i�B
i�B
jB
j�B
jB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
n/B
o B
oiB
oiB
o�B
o�B
pB
o�B
pB
pB
p;B
qB
qvB
qAB
qvB
qvB
rB
rB
q�B
rGB
rGB
rGB
r�B
sB
sMB
sMB
sMB
s�B
s�B
sMB
tB
s�B
s�B
s�B
t�B
u%B
uZB
u�B
v+B
uZB
u�B
u�B
u�B
v`B
v�B
v`B
w�B
wfB
v�B
wfB
x8B
w�B
w�B
x�B
x�B
xB
xB
xlB
yrB
y�B
zDB
y�B
z�B
z�B
z�B
z�B
{B
z�B
{JB
{JB
{�B
|B
{�B
|B
|�B
|B
{�B
|�B
|B
}"B
|PB
|�B
|�B
|�B
|�B
|�B
}�B
}VB
|�B
|�B
}�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
~�B
.B
.B
�B
cB
~�B
� B
�B
cB
�B
��B
�B
��B
��B
�oB
�B
�oB
�uB
��B
�uB
�AB
�GB
�GB
��B
�{B
�B
��B
��B
��B
��B
�{B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�MB
��B
�B
��B
��B
�%B
��B
�SB
��B
��B
��B
��B
��B
�YB
��B
�YB
�+B
��B
��B
�+B
�_B
��B
�_B
��B
��B
��B
��B�aB�[B��B�[B�-B�aB�OB�'B��B��B�'B��B�-B��B��B�-B��B��B�UB��B�3B��B�}B��B�[B�B�[B��B�wB��B�aB�B�aB�wB�CB�IB��B�B�B�nB�}B�B�eB��B��B��B��B�B�tB��B�_B��B�FB��B��B��B��B�hB�tB�@B��B�B��B��B��B�bB�bB�\B��B��B�VB��B�@B�!B��B��B��B��B�bB��B�VB��B��B��B�bB��B��B�-B�bB�-B��B��B��B�'B��B�4B��B��B��B��B��B��B�'B��B��B�B��B��B��B��B�CB�=B��B��B�wB��BȀBΥB��B�B�TB�B�B	
�B	&LB	@OB	x�B	��B	��B	��B	��B	��B	��B	�dB	�B	��B	�B	�WB	��B
DB
�B
MB
�B
MB
%B
�B
MB
�B
�B
GB
�B

rB
B
B
�B
B	��B	�cB
B
�B
�B

�B
4B
�B
B
�B
�B
1B
�B
_B
	B
�B
  B
 4B	�+B	�B	��B	��B	�B	��B	�B	��B	�rB	�vB	�KB	�B	�WB	��B	�KB	�"B	�B	��B	�yB	�B	��B	�B	�2B	�TB	�&B	�B	��B	�NB	��B	�DB	�B	�AB	�B	��B	��B	�vB	�B	��B
B

=B
�B
�B
�B
�B
�B
CB
�B
�B
qB
�B
�B
B
#:B
'�B
)_B
0�B
2�B
4�B
�oB
�:B
��B
� B
��B
��B
��B
�"B
��B
�=B
��B
��B
��B
�B
�	B
��B
��B
�7B
��B
�lB
�=B
�rB
�1B
�1B
�	B
�B
�fB
��B
��B
��B
�1B
��B
��B
�	B
�xB
��B
�lB
��B
�B
��B
�rB
�7B
��B
��B
��B
��B
��B
�oB
�4B
cB
.B
~�B
~]B
}�B
}VB
}VB
z�B
{�B
|PB
|�B
{JB
{B
y�B
z�B
{�B
|�B
{B
x�B
v�B
u%B
t�B
uZB
u�B
xB
}�B
sB
rGB
sMB
s�B
n�B
o5B
o5B
pB
ncB
� B
��B
v�B
rGB
qAB
oiB
r|B
sMB
p;B
p;B
r�B
~�B
x�B
~]B
jB
poB
o5B
m)B
m�B
h�B
i�B
v`B
u�B
r�B
rGB
q�B
w2B
x�B
p�B
h>B
iyB
l�B
e�B
f�B
aHB
dZB
a�B
_B
`B
\�B
\]B
[#B
Y�B
Z�B
[WB
XEB
^jB
y>B
x8B
poB
h
B
c�B
`BB
b�B
��B
�7B
��B
�hB
�VB
��B
�B
�B
�B
�qB
�B
�!B
��B
��B
�B
�B
�GB
��B
�>B�BuBeB4B<6B8�B9�B;�B<jB?B@BFtBD3BEBHKBK�BI�BLdBL0BGBRTBO�BP}BV�BiBgBc�Bh>Bi�BuZBv�BY�BQ�BYBZ�BW�BYB[#B[WB\)B`�Bh�Bp;BjBXyBV�BRTBP�Bb�B`�B^�Bc�BbNBbNBbBg�Bj�BaBXEBW�BW
BTaBS�BT,BU�BU2BT�BQNBS&BNBK�BH�BF�BGBI�BJXBHKBF?B:^BB�B@B*eB�BB
�B
��B
�B
�B
�B
�dB
��B
ɆB
�B
�FB
�!B
��B
��B
��B
� B
��B
�B
��B
��B
zxB
u�B
k�B
i�B
��B
h�B
[�B
B�B
7�B
^jB
>�B
}�B
J�B
B�B
A�B
eB
OB
 �B
qB
�B
kB
�B
"�B
N<B
8RB
A�B
*�B
,=B
�B
)_B
*0B
2�B
�B
fB
%B	��B	�xB	��B	��B	�%B	��B	�|B	��B	�B	�B	��B	�cB	�B	��B	�B	��B	� B	�]B	�B	�B	��B	�8B	�B	��B	��B	��B	�B	�/B	�yB	�`B	�B	�#B	�B	�mB	�9B	ӏB	ԕB	�&B	�}B	҉B	�QB	�B	�B	ǮB	ɆB	�}B	��B	�tB	�<B	�BB	��B	�UB	�nB	�B	��B	��B	�B	�OB	��B	��B	��B	��B	��B	��B	�1B	|�B	�B	��B	xlB	� B	t�B	zDB	t�B	y�B	rB	e,B	^�B	bNB	`vB	gmB	_;B	Y�B	e�B	[�B	\�B	T�B	T,B	WsB	VB	Q�B	H�B	D�B	CaB	H�B	K�B	K�B	J�B	@B	<�B	:�B	3�B	49B	1�B	:�B	B�B	A B	;�B	7�B	7�B	1�B	8B	5tB	1'B	.B	.B	;0B	,�B	)*B	2�B	+�B	2�B	*�B	)*B	.�B	;�B	B'B	>�B	1�B	4�B	3�B	1�B	.�B	2aB	/OB	33B	/�B	,B	+�B	+6B	+6B	+B	)_B	)*B	)�B	+kB	,�B	0!B	)�B	)�B	+B	'�B	>BB	9XB	4�B	+�B	*�B	'�B	-�B	A B	#:B	L0B	+B	7�B	*eB	-CB	;0B	4�B	-�B	)�B	'�B	(�B	'�B	#�B	%FB	#B	)�B	'RB	,qB	$�B	*�B	(�B	+kB	!�B	'�B	0�B	1'B	.�B	5?B	0�B	.B	0�B	2-B	33B	1�B	0UB	1�B	1�B	0�B	0UB	1�B	2aB	4nB	2aB	2�B	5tB	7�B	9�B	;�B	;�B	9�B	8�B	9�B	7�B	8�B	=B	;�B	8B	7�B	6B	8�B	5?B	6�B	<�B	8�B	7�B	8B	7�B	7B	6�B	8RB	8�B	6�B	7�B	8�B	7�B	7�B	9XB	8�B	8RB	8�B	:�B	9�B	9�B	;�B	;0B	:^B	;�B	<�B	<6B	=qB	?}B	>BB	>B	CaB	A B	@�B	@�B	A�B	A�B	@�B	B[B	B[B	A�B	A�B	C�B	E9B	F�B	F�B	GzB	IRB	IRB	H�B	H�B	I�B	I�B	IB	J�B	K^B	J�B	JXB	K)B	LdB	K�B	L0B	O�B	P�B	Q�B	S[B	W
B	XEB	[WB	\)B	\]B	^jB	aHB	a�B	`�B	a�B	bNB	aHB	b�B	e`B	dZB	c�B	d�B	e�B	c�B	e�B	f2B	dZB	e�B	f�B	e�B	e,B	f�B	e`B	d�B	f2B	g�B	e�B	e�B	ffB	d�B	d�B	f�B	f2B	f2B	gB	gB	i�B	i�B	n/B	z�B	d&B	u�B	u%B	ncB	�B	e�B	h�B	pB	k�B	m�B	n/B	o5B	o B	ncB	ncB	o5B	pB	pB	poB	r�B	tTB	t�B	t�B	v+B	v+B	w2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          B�AB��B�vB�vB��B��B��B��B�UB��B��B��B�!B��B��B�&B�NB�B�B��B��B�jB	]�B	՛B
-B
B
�B
{B
!B	�rB	��B	��B	�B	�'B
oB
=B
)yB
��B
�#B
�fB
��B
��B
r�B
�NB
��B7�BZBtnBr�Bj�BdB\CB_�B1B
�[B
�bB
{�B
G+B
A�B
1�B
 B	�B	�AB	�nB	��B	�fB	��B	�~B	�"B	�fB	�%B	o�B	iDB	\)B	PB	C�B	;0B	6zB	>(B	4�B	0B	4�B	?HB	>�B	:�B	.}B	4nB	5�B	4�B	8�B	?�B	:�B	9rB	?.B	D�B	I7B	NB	XEB	b4B	e�B	h$B	pUB	}�B	oB	q�B	y�B	��B	��B	{dB	z*B	w2B	zB	��B	�B	��B	�B	��B	��B	��B	�LB	��B	��B	��B	�YB	��B	��B	�vB	�FB	��B	�pB	�B	��B	�'B
B
�B
�B
�B
�B
�B
aB
�B
EB
�B
!�B
$�B
%,B
&B
'�B
(�B
)�B
,B
/�B
2-B
2|B
6FB
8B
5�B
5�B
4�B
5tB
2�B
2|B
1�B
2B
4nB
3�B
4�B
6�B
8�B
88B
7�B
7�B
6�B
88B
:�B
:�B
;�B
:�B
<�B
:B
:*B
:DB
:�B
;�B
=VB
=qB
<jB
>�B
C�B
CaB
@�B
@B
>]B
>(B
=<B
="B
:xB
:xB
:�B
;dB
;B
;B
;JB
<B
<B
;�B
<6B
=<B
>�B
=�B
>B
>wB
@�B
AUB
AUB
@�B
A B
@�B
AB
AB
AUB
A;B
B[B
<jB
:�B
7�B
7B
5�B
7�B
6FB
4nB
3�B
33B
33B
2GB
1�B
1�B
1B
0!B
0!B
/OB
.�B
.IB
,qB
,=B
+�B
*�B
*eB
*B
)*B
&�B
%,B
%�B
"�B
!�B
"B
!-B
pB
�B
B
�B
IB
]B
qB
B
)B
�B
�B
�B
�B
=B
�B
7B
�B
KB
�B
B
WB
eB
_B
�B
+B
B
�B
�B
�B
YB
YB
�B
�B
SB
9B
+B
�B
�B
B
�B
�B
&B
�B
�B
�B
B
:B
�B
 B
B
B
(B
VB
VB
B
�B
B
jB
B
�B
dB
dB
JB
dB
~B
�B
PB
jB
�B
B
~B
dB
~B
JB
�B
�B
~B
0B
B
�B
�B
�B
VB
�B
dB
�B
6B
JB
~B
~B
B
<B
�B
PB
�B
B
jB
�B
�B
�B
�B
�B
dB
�B
B
JB
B
dB
B
�B
B
�B
B
BB
�B
�B
�B
B
4B
�B
�B
�B
�B
�B
FB
�B
�B
�B
�B
B
TB
�B
�B
�B
TB
 B
hB
�B
�B
�B
[B
oB
 B
�B
�B
TB
uB
 B
B
�B
�B
�B
�B
�B
�B
�B
�B
MB
�B
sB
�B
SB
�B

B
�B

B
�B
�B
�B
�B
+B
yB
�B
B
_B
�B
�B
�B
B
B
B
�B
B
B
B
�B
�B
QB
7B
kB
kB
�B
kB
�B
�B
B
�B
�B
QB
�B
�B
	B
�B
�B
5B
B
�B
B
OB
jB
�B
;B
 'B
�B
�B
 \B
 B
 �B
 �B
!|B
!�B
!bB
!�B
"hB
#B
"�B
"�B
"�B
# B
#�B
$�B
$�B
$tB
$�B
%,B
%`B
%�B
'RB
'B
'B
'RB
'B
'�B
'mB
'mB
'8B
'RB
'�B
(>B
(>B
(�B
)DB
)B
)_B
)DB
)DB
)_B
)DB
)_B
)DB
)�B
)�B
)DB
)_B
)_B
)�B
*B
*�B
*eB
*B
*�B
+B
+6B
+�B
+�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+B
,B
+�B
+kB
+�B
+�B
+�B
+�B
+�B
,�B
,�B
-)B
,�B
-B
,�B
,�B
-]B
-wB
-�B
-�B
-�B
-�B
-�B
./B
.B
-�B
.�B
1'B
0UB
/�B
0;B
0�B
0�B
0�B
1[B
1'B
1AB
2aB
1�B
1�B
1�B
3MB
3�B
5B
4�B
4�B
4�B
5�B
5�B
5�B
5tB
5ZB
6zB
6�B
7B
7LB
7LB
8B
7�B
8�B
8�B
9XB
9�B
:xB
:�B
:*B
:xB
:�B
;B
;�B
;B
;0B
;JB
<PB
;�B
;�B
<jB
<�B
<jB
<�B
=�B
>BB
>(B
>(B
>�B
?HB
?cB
?cB
?HB
?�B
@ B
?�B
@B
A B
A B
A�B
A;B
A;B
AoB
A�B
A B
AB
AoB
A�B
B�B
C�B
B�B
B�B
B�B
CB
CGB
CB
C-B
C{B
C�B
DB
DMB
D�B
EB
E�B
FYB
F�B
G�B
HB
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J	B
J�B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
LJB
MB
M6B
M�B
M�B
NB
N<B
N�B
O\B
O(B
OB
OB
O�B
O�B
PB
PHB
O�B
P}B
P�B
Q B
Q B
Q�B
RTB
R�B
R�B
R�B
SuB
SB
SB
SB
S@B
SuB
SuB
S�B
TaB
UgB
VB
T�B
T�B
UMB
VB
VSB
V9B
V�B
W?B
V�B
V�B
V�B
V�B
V�B
WsB
W�B
X_B
X+B
X�B
X�B
X�B
Y�B
YB
YeB
Y�B
ZQB
ZQB
[	B
[=B
[=B
[�B
\B
\]B
\�B
\�B
\�B
]/B
]/B
\�B
]�B
^�B
]�B
^5B
^�B
_B
`B
_VB
_�B
_�B
_�B
_�B
aB
a|B
a�B
a�B
a�B
bB
a�B
bB
bhB
b�B
cB
cB
cnB
cB
cB
cTB
c�B
c�B
c�B
dZB
d&B
d&B
dtB
d�B
d�B
e�B
e�B
e�B
e�B
e�B
fB
e�B
fB
fLB
fB
fB
fLB
f�B
gmB
gmB
g�B
h
B
g�B
h$B
h$B
hXB
h�B
i*B
iDB
i_B
iyB
i�B
iyB
iyB
i�B
j0B
jKB
j�B
j�B
kB
lB
k�B
k�B
l=B
l=B
l�B
m)B
mB
m)B
m]B
m�B
ncB
n/B
n�B
o5B
o�B
o�B
o�B
p!B
p!B
o�B
p!B
p;B
p�B
q'B
q�B
q[B
q�B
rGB
r|B
rGB
r-B
r�B
r�B
r�B
r�B
s�B
shB
shB
s�B
s�B
s�B
s�B
t�B
tB
s�B
tB
uB
utB
u�B
vFB
v`B
utB
u�B
u�B
vB
v�B
v�B
v�B
w�B
wfB
v�B
w�B
x�B
w�B
w�B
x�B
x�B
xB
xB
x�B
y�B
z*B
z�B
zDB
z�B
z�B
z�B
{0B
{�B
z�B
{�B
{B
{�B
|6B
{�B
|PB
|�B
|B
|B
|�B
|jB
}"B
|jB
|�B
}B
|�B
|�B
}"B
}�B
}VB
|�B
|�B
}�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~�B
HB
HB
�B
}B
HB
�B
�B
�B
�B
��B
� B
��B
��B
�oB
�;B
��B
��B
�B
��B
��B
�{B
�{B
��B
�{B
�aB
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�B
��B
��B
��B
�gB
��B
�9B
��B
�SB
�?B
��B
�mB
��B
��B
��B
�B
�B
��B
��B
��B
�+B
��B
�B
�EB
�_B
�B
��B
��B
��B
��G�O�B�aB�[B��B�[B�-B�aB�OB�'B��B��B�'B��B�-B��B��B�-B��B��B�UB��B�3B��B�}B��B�[B�B�[B��B�wB��B�aB�B�aB�wB�CB�IB��B�B�B�nB�}B�B�eB��B��B��B��B�B�tB��B�_B��B�FB��B��B��B��B�hB�tB�@B��B�B��B��B��B�bB�bB�\B��B��B�VB��B�@B�!B��B��B��B��B�bB��B�VB��B��B��B�bB��B��B�-B�bB�-B��B��B��B�'B��B�4B��B��B��B��B��B��B�'B��B��B�B��B��B��B��B�CB�=B��B��B�wB��BȀBΥB��B�B�TB�B�B	
�B	&LB	@OB	x�B	��B	��B	��B	��B	��B	��B	�dB	�B	��B	�B	�WB	��B
DB
�B
MB
�B
MB
%B
�B
MB
�B
�B
GB
�B

rB
B
B
�B
B	��B	�cB
B
�B
�B

�B
4B
�B
B
�B
�B
1B
�B
_B
	B
�B
  B
 4B	�+B	�B	��B	��B	�B	��B	�B	��B	�rB	�vB	�KB	�B	�WB	��B	�KB	�"B	�B	��B	�yB	�B	��B	�B	�2B	�TB	�&B	�B	��B	�NB	��B	�DB	�B	�AB	�B	��B	��B	�vB	�B	��B
B

=B
�B
�B
�B
�B
�B
CB
�B
�B
qB
�B
�B
B
#:B
'�B
)_B
0�B
2�B
4�B
�oB
�:B
��B
� B
��B
��B
��B
�"B
��B
�=B
��B
��B
��B
�B
�	B
��B
��B
�7B
��B
�lB
�=B
�rB
�1B
�1B
�	B
�B
�fB
��B
��B
��B
�1B
��B
��B
�	B
�xB
��B
�lB
��B
�B
��B
�rB
�7B
��B
��B
��B
��B
��B
�oB
�4B
cB
.B
~�B
~]B
}�B
}VB
}VB
z�B
{�B
|PB
|�B
{JB
{B
y�B
z�B
{�B
|�B
{B
x�B
v�B
u%B
t�B
uZB
u�B
xB
}�B
sB
rGB
sMB
s�B
n�B
o5B
o5B
pB
ncB
� B
��B
v�B
rGB
qAB
oiB
r|B
sMB
p;B
p;B
r�B
~�B
x�B
~]B
jB
poB
o5B
m)B
m�B
h�B
i�B
v`B
u�B
r�B
rGB
q�B
w2B
x�B
p�B
h>B
iyB
l�B
e�B
f�B
aHB
dZB
a�B
_B
`B
\�B
\]B
[#B
Y�B
Z�B
[WB
XEB
^jB
y>B
x8B
poB
h
B
c�B
`BB
b�B
��B
�7B
��B
�hB
�VB
��B
�B
�B
�B
�qB
�B
�!B
��B
��B
�B
�B
�GB
��B
�>B�BuBeB4B<6B8�B9�B;�B<jB?B@BFtBD3BEBHKBK�BI�BLdBL0BGBRTBO�BP}BV�BiBgBc�Bh>Bi�BuZBv�BY�BQ�BYBZ�BW�BYB[#B[WB\)B`�Bh�Bp;BjBXyBV�BRTBP�Bb�B`�B^�Bc�BbNBbNBbBg�Bj�BaBXEBW�BW
BTaBS�BT,BU�BU2BT�BQNBS&BNBK�BH�BF�BGBI�BJXBHKBF?B:^BB�B@B*eB�BB
�B
��B
�B
�B
�B
�dB
��B
ɆB
�B
�FB
�!B
��B
��B
��B
� B
��B
�B
��B
��B
zxB
u�B
k�B
i�B
��B
h�B
[�B
B�B
7�B
^jB
>�B
}�B
J�B
B�B
A�B
eB
OB
 �B
qB
�B
kB
�B
"�B
N<B
8RB
A�B
*�B
,=B
�B
)_B
*0B
2�B
�B
fB
%B	��B	�xB	��B	��B	�%B	��B	�|B	��B	�B	�B	��B	�cB	�B	��B	�B	��B	� B	�]B	�B	�B	��B	�8B	�B	��B	��B	��B	�B	�/B	�yB	�`B	�B	�#B	�B	�mB	�9B	ӏB	ԕB	�&B	�}B	҉B	�QB	�B	�B	ǮB	ɆB	�}B	��B	�tB	�<B	�BB	��B	�UB	�nB	�B	��B	��B	�B	�OB	��B	��B	��B	��B	��B	��B	�1B	|�B	�B	��B	xlB	� B	t�B	zDB	t�B	y�B	rB	e,B	^�B	bNB	`vB	gmB	_;B	Y�B	e�B	[�B	\�B	T�B	T,B	WsB	VB	Q�B	H�B	D�B	CaB	H�B	K�B	K�B	J�B	@B	<�B	:�B	3�B	49B	1�B	:�B	B�B	A B	;�B	7�B	7�B	1�B	8B	5tB	1'B	.B	.B	;0B	,�B	)*B	2�B	+�B	2�B	*�B	)*B	.�B	;�B	B'B	>�B	1�B	4�B	3�B	1�B	.�B	2aB	/OB	33B	/�B	,B	+�B	+6B	+6B	+B	)_B	)*B	)�B	+kB	,�B	0!B	)�B	)�B	+B	'�B	>BB	9XB	4�B	+�B	*�B	'�B	-�B	A B	#:B	L0B	+B	7�B	*eB	-CB	;0B	4�B	-�B	)�B	'�B	(�B	'�B	#�B	%FB	#B	)�B	'RB	,qB	$�B	*�B	(�B	+kB	!�B	'�B	0�B	1'B	.�B	5?B	0�B	.B	0�B	2-B	33B	1�B	0UB	1�B	1�B	0�B	0UB	1�B	2aB	4nB	2aB	2�B	5tB	7�B	9�B	;�B	;�B	9�B	8�B	9�B	7�B	8�B	=B	;�B	8B	7�B	6B	8�B	5?B	6�B	<�B	8�B	7�B	8B	7�B	7B	6�B	8RB	8�B	6�B	7�B	8�B	7�B	7�B	9XB	8�B	8RB	8�B	:�B	9�B	9�B	;�B	;0B	:^B	;�B	<�B	<6B	=qB	?}B	>BB	>B	CaB	A B	@�B	@�B	A�B	A�B	@�B	B[B	B[B	A�B	A�B	C�B	E9B	F�B	F�B	GzB	IRB	IRB	H�B	H�B	I�B	I�B	IB	J�B	K^B	J�B	JXB	K)B	LdB	K�B	L0B	O�B	P�B	Q�B	S[B	W
B	XEB	[WB	\)B	\]B	^jB	aHB	a�B	`�B	a�B	bNB	aHB	b�B	e`B	dZB	c�B	d�B	e�B	c�B	e�B	f2B	dZB	e�B	f�B	e�B	e,B	f�B	e`B	d�B	f2B	g�B	e�B	e�B	ffB	d�B	d�B	f�B	f2B	f2B	gB	gB	i�B	i�B	n/B	z�B	d&B	u�B	u%B	ncB	�B	e�B	h�B	pB	k�B	m�B	n/B	o5B	o B	ncB	ncB	o5B	pB	pB	poB	r�B	tTB	t�B	t�B	v+B	v+B	w2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5Vp<#�
<#�
<#�
<#�
<#�
<#�
<,
0<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%�<�7�<td<��<�f<�DM<��T<d"<(�4<���<��?<��3<b�7<#�
<0�x<�
�<��n<�#
<�]=�4<�7�<���<_��<#�
<#�
<#�
<-��<#�
<�i�<��V<�_�<��<�4�<M�<#�
<Wg�<`�/<4Ӄ<+�+<#�
<#�
<3D�<#�
<#�
<#�
<js�<�e#<4Ӄ<#�
<7�J<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-<]��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018092907285520180929072855IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100907010920181009070109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100907010920181009070109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551120190521075511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                