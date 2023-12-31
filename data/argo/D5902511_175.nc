CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-07T17:30:33Z creation; 2022-02-04T23:30:02Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � <�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � cH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210707173033  20220204223514  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_175                 6810_008521_175                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ق(cI{t@ق(cI{t11  @ق(�t�j@ق(�t�j@2^�y�@2^�y��e���=�e���=11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@G�@��
@�  @�  @�G�A ��A\)A   A+�A?\)A^{A\)A�Q�A�  A��A�  A�  A�  A�  A��B  B(�B�
B�B'�
B/�
B8  B@(�BH(�BPQ�BX  B`  Bh  Bp  Bx  B�
B�{B�{B�  B�  B�{B�  B�  B�(�B�{B�  B�  B�  B�  B��
B��B�B��B�  B�{B�{B�  B�  B��B��B�  B�  B��B�{B��B��B�(�C {C  C�C  C  C	��C�C�C  C  C  C  C  C
=C
=C  C   C"
=C$  C%�C(  C*  C,
=C-��C0
=C2�C4{C6
=C7��C9��C<  C>  C@  CB
=CD
=CF  CG�CI��CL{CN�CP{CQ��CS�HCU��CX  CZ  C\
=C]��C`
=Ca��Cd  Ce�Ch{Cj  Cl  Cn
=Cp  Cr  Ct  Cv  Cx
=Cz  C{��C}��C�  C���C�C���C�C�C�C�
=C�  C�
=C�  C�C�
=C�C�  C�C�C�C���C���C�  C�  C���C���C���C�
=C�
=C�  C���C���C�  C�  C�  C�  C�C�  C�  C�C�C�  C�  C���C�  C���C�  C�  C�C�C�
=C�  C�
=C�  C�  C���C���C�  C�  C���C�C���C�C�  C�  C�  C�C�C�C�  C�C�C�  C�  C�  C���C���C���C�  C���C���C�C�  C���C�C�C���C�C�C�  C���C�  C���C���C���C�  C���C�  C�C���C�C�
=C���C���C�
=C�C�  C�C�C�C�  C�  C�C�  C�C���C�  C�C�C�  C�  C���C�C�C���C���C���C�C�  C�C���D � D  D}qD  D}qD�D}qD  D}qD��D� D�D��D�qDz�D  D��D	  D	z�D	�qD
}qD
��D}qD�qD� D�D� D  D}qD�qDz�D��D� D�D}qD  D��D�qD� D  D� D  D��D�D}qD  D� D  D}qD��Dz�D�qD��D�D� D  D��D  D� D�D}qD�qD}qD��D }qD �qD!}qD"�D"��D#  D#�D$�D$� D$�qD%��D&�D&� D'�D'�D(  D(}qD)  D)� D*�D*��D+  D+� D+�qD,� D-  D-� D-�qD.� D/  D/}qD0  D0}qD0�qD1� D2�D2��D3�D3� D4  D4}qD5  D5� D5��D6� D7�D7}qD7�qD8}qD9  D9��D:  D:� D:�qD;� D<�D<}qD<�qD=� D=�qD>}qD>�qD?� D@�D@� D@�qDAz�DB  DB��DC�DC��DC�qDD}qDD�qDE��DF�DF}qDG  DG� DG��DHz�DH��DI� DJDJ� DK  DK� DL�DL�DM�DM}qDN�DN� DO  DO� DO�qDP}qDQ�DQ� DQ�qDR� DSDS��DS��DT� DUDU�DVDV� DV�qDW��DX�DX}qDY  DY�DZ�DZ}qD[  D[� D[�qD\��D]�D]� D^  D^��D_  D_� D_��D`� Da�Da��Da�qDb}qDc  Dc� Dd  Dd��Dd�qDe� Df  Df� DgDg��Dh�Dh��Di�Di� Dj  Dj�Dk�Dk� Dk��Dl� Dm�Dm� Dn  Dn}qDo�Do}qDo�qDp� Dq�Dq� Dq�qDr� Dr��Ds}qDt  Dt��Du�Du� Du�qDv}qDw  Dw}qDw�qDx� Dx�qDy}qDz�Dz��Dz�qD{� D|�D|��D}�D}� D}�qD~� D  D}qD��D�>�D��HD��HD���D�>�D��HD�D��D�@ D�~�D���D��qD�@ D��HD��HD���D�@ D���D��HD�HD�AHD��HD��HD�HD�B�D���D�D�  D�>�D�� D�� D�HD�AHD���D��HD��D�@ D�~�D���D�  D�@ D�}qD���D�  D�AHD�~�D�� D��D�B�D���D��HD�  D�>�D�� D�� D��D�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D��qD���D�@ D��HD�D�HD�AHD�~�D���D�HD�B�D�� D���D�  D�>�D�� D���D���D�AHD�� D��HD�  D�>�D�~�D�� D�HD�>�D�}qD���D�  D�AHD�~�D���D���D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD��HD��HD���D�>�D�� D���D�  D�AHD�� D��qD���D�@ D�~�D�� D�HD�B�D�� D�� D�  D�>�D��HD��HD��qD�=qD��HD��HD�  D�=qD�� D�� D���D�AHD���D�D���D�>�D�� D�� D�  D�B�D�� D�D��D�AHD��HD���D�HD�B�D��HD�D��D�AHD��HD�� D�  D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD��HD���D���D�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD��HD�� D��qD�>�D�~�D���D��qD�>�D�� D��qD���D�B�D���D�� D���D�@ D�� D��HD�HD�=qD�|)D��qD�  D�AHD��HD�� D�HD�AHD�� D���D�  D�AHD�� D��qD�HD�AHD�~�D�� D�  D�>�D�~�D��qD�HD�AHD�� D��HD�  D�>�D�� D��HD�HD�>�D�� D�� D��qD�>�D�� D��HD�HD�>�D�}qD��qD�  D�B�D�� D�� D�HD�@ D���D��HD�  D�AHD�� D���D�  D�@ D�� D���D�HD�B�DHD�D�HD�@ DÀ D�� D���D�>�D�~�D�� D�  D�@ Dŀ D�� D���D�>�Dƀ D�� D�  D�=qD�~�D�� D���D�>�DȀ D�� D�  D�>�D�~�D�� D���D�@ Dʀ Dʾ�D�  D�=qD�~�D�� D�HD�@ D̀ D��HD�  D�@ D̀ D�� D�  D�@ D΁HD��HD�  D�@ DρHD��HD��D�@ D�~�D��HD�  D�>�DсHD�� D���D�@ DҀ DҾ�D�  D�AHDӀ D��HD�HD�>�D�~�DԾ�D���D�AHDՁHD��HD�  D�>�D�~�D��HD�HD�@ DׁHD��HD�  D�>�D�~�D��HD�HD�@ DفHD��HD�  D�>�DځHD��HD�  D�@ D�~�D�� D���D�@ D܀ D�� D�  D�@ D݀ D�� D�HD�AHDށHD��HD���D�>�D߀ D��HD���D�@ D��HD�� D�  D�@ D� D��HD�  D�@ D� D��HD��D�@ D� D��HD���D�=qD�~�D�� D�HD�@ D�}qD�)D��qD�>�D� D�� D��qD�@ D� D羸D�  D�>�D�}qD�qD��qD�=qD�~�D�� D�  D�@ D� D꾸D��qD�@ D� D뾸D���D�>�D�}qD�� D��D�AHD�~�D��qD���D�>�D� D�� D�HD�@ D�~�DﾸD�HD�B�D���D�D�HD�AHD� D��HD�  D�>�D� D�D���D�@ D�HD�D�  D�AHD� D���D�  D�B�D���D��HD�HD�+�>��
>�G�?8Q�?��
?\?�@\)@.{@E�@Q�@p��@��
@�{@���@��@�z�@�p�@���@�z�@��
@���@��HA�
A	��A\)AAp�A#33A(Q�A/\)A6ffA<(�AAG�AHQ�AP��AUA[�Ab�\Ai��Ap  Au�Az�HA���A���A�\)A��HA�ffA�G�A��A�\)A��HA��A��A�33A�A�  A��\A�A�Q�A�=qA�(�A�
=A�G�A�33A���A�
=A���A\A�(�AƸRA�Q�A�G�A��HA�p�A�{AϮAљ�AӅA�z�A�p�A�  A�G�A�=qA�(�A�{A�\)A�Q�A�=qA�z�A�A�
=A���A��HA�(�A��A�
=A�G�A�=qA�A�A�\)A���A���A�33A�p�A�ffA��B ��BB=qB�HB  B��B��B=qB�HB  B��B	G�B
=qB33B�
BQ�Bp�B=qB33B�BQ�Bp�B=qB�RB\)BQ�Bp�B�B�RB�
B��Bp�B=qB�Bz�B�B{B\)B Q�B ��B"{B#33B$(�B$��B%�B'
=B(Q�B(��B)B+33B,(�B,��B-��B/
=B0(�B0��B1��B2�HB4  B4��B5B6�HB7�
B8��B9B;
=B;�
B<��B=B?
=B@  B@��BABC
=BC�
BD��BEBG
=BG�BH��BI�BJ�HBK�BL��BMBN�\BO\)BPz�BQ��BR�\BS33BT(�BUp�BVffBW33BX(�BYp�BZffB[33B\  B]G�B^ffB_33B`  B`��Bb=qBc\)Bd  Bd��Bf=qBg33Bg�
Bh��Bi�Bk
=Bk�
Bl��Bm��Bn�HBo�Bpz�BqG�Br=qBs\)Btz�BuG�Bu�Bw
=Bx(�Bx��By��Bz�\B{�B|��B}p�B~=qB�B�=qB���B�
=B��B�=qB���B���B�\)B��B�z�B���B�G�B��B�=qB��RB�
=B��B�  B��\B��B��B��
B�ffB�
=B��B��B�Q�B���B���B�{B��\B��HB�p�B�  B��\B�
=B�\)B��
B�z�B���B�\)B�B�=qB��HB�\)B�B�(�B���B�33B��
B�Q�B���B�33B�B�Q�B��HB�33B���B�(�B���B�G�B���B�{B���B��B�p�B��B�ffB�
=B��B��
B�=qB���B�G�B�B�(�B��\B�33B��B�=qB��\B���B��B�(�B���B�
=B��B�=qB���B�\)B��
B�Q�B���B��B�(�B���B�
=B��B�(�B���B�G�B��B�Q�B���B��B��B�z�B�33B�B�(�B��RB�G�B�  B�z�B���B�p�B�{B��RB�G�B��B�(�B��RB�p�B�  B��\B�
=B��B�{B���B�p�B�  B�z�B���BÅB�(�B���B�\)B�B�Q�B�
=BǮB�Q�B���B�33B�  Bʣ�B�G�B�B�=qB��HB�p�B�(�B���B�33B�B�z�B��BѮB�(�Bң�B�\)B�  B�z�B�
=B�p�B�{BָRB�\)B��
B�=qB��HBٙ�B�(�Bڣ�B��BۮB�Q�B���Bݙ�B�{B�z�B�33B��
B�Q�B��HBᙚB�(�B��B�33B��B�\B���B�B�=qB���B�33B�B�z�B�
=B�B�  B�\B�\)B��B�ffB��HB�p�B�(�B���B�\)B��
B�z�B��B�B�{B��B�p�B�  B�ffB�
=B�B�Q�B��RB�G�B�  B��\B���B��B�(�B��HB�\)B�B�z�B��B��B�  B��RB�G�B���C (�C z�C �RC �C=qC�\C��C  CQ�C��C��C(�C\)CC
=C33C�C�HC�C\)C��C�C=qC�C�C��CQ�C��CC
=CQ�C��C�C	33C	ffC	��C
  C
Q�C
z�C
�RC{CQ�C�\CC  CQ�C��C�HC(�CQ�C��C��C(�CffC�RC
=CQ�C�\C��C�Cp�C�C�C=qC�\C��C
=CQ�C��C��C(�CffC��C  CG�C��C�
C{Cp�CC
=CG�C�\C�C=qCp�C�RC
=Cp�C�C�C=qC��C��C33Cz�CC(�Cp�C�RC��CQ�C�C�C(�Cp�C�RC{CffC�C�CG�C��C�
C �C p�C C!
=C!G�C!�\C!�HC"=qC"z�C"�RC#  C#Q�C#��C#�C$�C$ffC$�RC%
=C%=qC%z�C%��C&�C&ffC&��C&�HC'=qC'�\C'C(  C(\)C(�C)  C)33C)p�C)��C*�C*ffC*��C+  C+Q�C+��C+�
C,33C,�C,��C-
=C-Q�C-��C-��C.G�C.��C.�C/(�C/p�C/��C0(�C0p�C0�RC1  C1Q�C1�C2
=C2Q�C2�\C2�
C3�C3�C3�
C4{C4ffC4�C5  C5Q�C5�C5�C633C6�C6�HC7=qC7�\C7��C8{C8ffC8C9{C9ffC9��C9�C:33C:�C:�
C;33C;�C;C<
=C<\)C<��C=  C=Q�C=�C=�C>(�C>�C>�
C?33C?�C?�
C@{C@\)C@��C@��CA\)CA�CA��CB=qCB�\CB�HCC33CC�CC�CD=qCD�\CD�
CE{CEffCE�RCF{CFffCF�RCG  CGG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        ?�  ?��H@G�@��
@�  @�  @�G�A ��A\)A   A+�A?\)A^{A\)A�Q�A�  A��A�  A�  A�  A�  A��B  B(�B�
B�B'�
B/�
B8  B@(�BH(�BPQ�BX  B`  Bh  Bp  Bx  B�
B�{B�{B�  B�  B�{B�  B�  B�(�B�{B�  B�  B�  B�  B��
B��B�B��B�  B�{B�{B�  B�  B��B��B�  B�  B��B�{B��B��B�(�C {C  C�C  C  C	��C�C�C  C  C  C  C  C
=C
=C  C   C"
=C$  C%�C(  C*  C,
=C-��C0
=C2�C4{C6
=C7��C9��C<  C>  C@  CB
=CD
=CF  CG�CI��CL{CN�CP{CQ��CS�HCU��CX  CZ  C\
=C]��C`
=Ca��Cd  Ce�Ch{Cj  Cl  Cn
=Cp  Cr  Ct  Cv  Cx
=Cz  C{��C}��C�  C���C�C���C�C�C�C�
=C�  C�
=C�  C�C�
=C�C�  C�C�C�C���C���C�  C�  C���C���C���C�
=C�
=C�  C���C���C�  C�  C�  C�  C�C�  C�  C�C�C�  C�  C���C�  C���C�  C�  C�C�C�
=C�  C�
=C�  C�  C���C���C�  C�  C���C�C���C�C�  C�  C�  C�C�C�C�  C�C�C�  C�  C�  C���C���C���C�  C���C���C�C�  C���C�C�C���C�C�C�  C���C�  C���C���C���C�  C���C�  C�C���C�C�
=C���C���C�
=C�C�  C�C�C�C�  C�  C�C�  C�C���C�  C�C�C�  C�  C���C�C�C���C���C���C�C�  C�C���D � D  D}qD  D}qD�D}qD  D}qD��D� D�D��D�qDz�D  D��D	  D	z�D	�qD
}qD
��D}qD�qD� D�D� D  D}qD�qDz�D��D� D�D}qD  D��D�qD� D  D� D  D��D�D}qD  D� D  D}qD��Dz�D�qD��D�D� D  D��D  D� D�D}qD�qD}qD��D }qD �qD!}qD"�D"��D#  D#�D$�D$� D$�qD%��D&�D&� D'�D'�D(  D(}qD)  D)� D*�D*��D+  D+� D+�qD,� D-  D-� D-�qD.� D/  D/}qD0  D0}qD0�qD1� D2�D2��D3�D3� D4  D4}qD5  D5� D5��D6� D7�D7}qD7�qD8}qD9  D9��D:  D:� D:�qD;� D<�D<}qD<�qD=� D=�qD>}qD>�qD?� D@�D@� D@�qDAz�DB  DB��DC�DC��DC�qDD}qDD�qDE��DF�DF}qDG  DG� DG��DHz�DH��DI� DJDJ� DK  DK� DL�DL�DM�DM}qDN�DN� DO  DO� DO�qDP}qDQ�DQ� DQ�qDR� DSDS��DS��DT� DUDU�DVDV� DV�qDW��DX�DX}qDY  DY�DZ�DZ}qD[  D[� D[�qD\��D]�D]� D^  D^��D_  D_� D_��D`� Da�Da��Da�qDb}qDc  Dc� Dd  Dd��Dd�qDe� Df  Df� DgDg��Dh�Dh��Di�Di� Dj  Dj�Dk�Dk� Dk��Dl� Dm�Dm� Dn  Dn}qDo�Do}qDo�qDp� Dq�Dq� Dq�qDr� Dr��Ds}qDt  Dt��Du�Du� Du�qDv}qDw  Dw}qDw�qDx� Dx�qDy}qDz�Dz��Dz�qD{� D|�D|��D}�D}� D}�qD~� D  D}qD��D�>�D��HD��HD���D�>�D��HD�D��D�@ D�~�D���D��qD�@ D��HD��HD���D�@ D���D��HD�HD�AHD��HD��HD�HD�B�D���D�D�  D�>�D�� D�� D�HD�AHD���D��HD��D�@ D�~�D���D�  D�@ D�}qD���D�  D�AHD�~�D�� D��D�B�D���D��HD�  D�>�D�� D�� D��D�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D��qD���D�@ D��HD�D�HD�AHD�~�D���D�HD�B�D�� D���D�  D�>�D�� D���D���D�AHD�� D��HD�  D�>�D�~�D�� D�HD�>�D�}qD���D�  D�AHD�~�D���D���D�@ D�� D��HD�HD�>�D�~�D��HD�HD�AHD��HD��HD���D�>�D�� D���D�  D�AHD�� D��qD���D�@ D�~�D�� D�HD�B�D�� D�� D�  D�>�D��HD��HD��qD�=qD��HD��HD�  D�=qD�� D�� D���D�AHD���D�D���D�>�D�� D�� D�  D�B�D�� D�D��D�AHD��HD���D�HD�B�D��HD�D��D�AHD��HD�� D�  D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD��HD���D���D�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD��HD�� D��qD�>�D�~�D���D��qD�>�D�� D��qD���D�B�D���D�� D���D�@ D�� D��HD�HD�=qD�|)D��qD�  D�AHD��HD�� D�HD�AHD�� D���D�  D�AHD�� D��qD�HD�AHD�~�D�� D�  D�>�D�~�D��qD�HD�AHD�� D��HD�  D�>�D�� D��HD�HD�>�D�� D�� D��qD�>�D�� D��HD�HD�>�D�}qD��qD�  D�B�D�� D�� D�HD�@ D���D��HD�  D�AHD�� D���D�  D�@ D�� D���D�HD�B�DHD�D�HD�@ DÀ D�� D���D�>�D�~�D�� D�  D�@ Dŀ D�� D���D�>�Dƀ D�� D�  D�=qD�~�D�� D���D�>�DȀ D�� D�  D�>�D�~�D�� D���D�@ Dʀ Dʾ�D�  D�=qD�~�D�� D�HD�@ D̀ D��HD�  D�@ D̀ D�� D�  D�@ D΁HD��HD�  D�@ DρHD��HD��D�@ D�~�D��HD�  D�>�DсHD�� D���D�@ DҀ DҾ�D�  D�AHDӀ D��HD�HD�>�D�~�DԾ�D���D�AHDՁHD��HD�  D�>�D�~�D��HD�HD�@ DׁHD��HD�  D�>�D�~�D��HD�HD�@ DفHD��HD�  D�>�DځHD��HD�  D�@ D�~�D�� D���D�@ D܀ D�� D�  D�@ D݀ D�� D�HD�AHDށHD��HD���D�>�D߀ D��HD���D�@ D��HD�� D�  D�@ D� D��HD�  D�@ D� D��HD��D�@ D� D��HD���D�=qD�~�D�� D�HD�@ D�}qD�)D��qD�>�D� D�� D��qD�@ D� D羸D�  D�>�D�}qD�qD��qD�=qD�~�D�� D�  D�@ D� D꾸D��qD�@ D� D뾸D���D�>�D�}qD�� D��D�AHD�~�D��qD���D�>�D� D�� D�HD�@ D�~�DﾸD�HD�B�D���D�D�HD�AHD� D��HD�  D�>�D� D�D���D�@ D�HD�D�  D�AHD� D���D�  D�B�D���D��HD�HG�O�>��
>�G�?8Q�?��
?\?�@\)@.{@E�@Q�@p��@��
@�{@���@��@�z�@�p�@���@�z�@��
@���@��HA�
A	��A\)AAp�A#33A(Q�A/\)A6ffA<(�AAG�AHQ�AP��AUA[�Ab�\Ai��Ap  Au�Az�HA���A���A�\)A��HA�ffA�G�A��A�\)A��HA��A��A�33A�A�  A��\A�A�Q�A�=qA�(�A�
=A�G�A�33A���A�
=A���A\A�(�AƸRA�Q�A�G�A��HA�p�A�{AϮAљ�AӅA�z�A�p�A�  A�G�A�=qA�(�A�{A�\)A�Q�A�=qA�z�A�A�
=A���A��HA�(�A��A�
=A�G�A�=qA�A�A�\)A���A���A�33A�p�A�ffA��B ��BB=qB�HB  B��B��B=qB�HB  B��B	G�B
=qB33B�
BQ�Bp�B=qB33B�BQ�Bp�B=qB�RB\)BQ�Bp�B�B�RB�
B��Bp�B=qB�Bz�B�B{B\)B Q�B ��B"{B#33B$(�B$��B%�B'
=B(Q�B(��B)B+33B,(�B,��B-��B/
=B0(�B0��B1��B2�HB4  B4��B5B6�HB7�
B8��B9B;
=B;�
B<��B=B?
=B@  B@��BABC
=BC�
BD��BEBG
=BG�BH��BI�BJ�HBK�BL��BMBN�\BO\)BPz�BQ��BR�\BS33BT(�BUp�BVffBW33BX(�BYp�BZffB[33B\  B]G�B^ffB_33B`  B`��Bb=qBc\)Bd  Bd��Bf=qBg33Bg�
Bh��Bi�Bk
=Bk�
Bl��Bm��Bn�HBo�Bpz�BqG�Br=qBs\)Btz�BuG�Bu�Bw
=Bx(�Bx��By��Bz�\B{�B|��B}p�B~=qB�B�=qB���B�
=B��B�=qB���B���B�\)B��B�z�B���B�G�B��B�=qB��RB�
=B��B�  B��\B��B��B��
B�ffB�
=B��B��B�Q�B���B���B�{B��\B��HB�p�B�  B��\B�
=B�\)B��
B�z�B���B�\)B�B�=qB��HB�\)B�B�(�B���B�33B��
B�Q�B���B�33B�B�Q�B��HB�33B���B�(�B���B�G�B���B�{B���B��B�p�B��B�ffB�
=B��B��
B�=qB���B�G�B�B�(�B��\B�33B��B�=qB��\B���B��B�(�B���B�
=B��B�=qB���B�\)B��
B�Q�B���B��B�(�B���B�
=B��B�(�B���B�G�B��B�Q�B���B��B��B�z�B�33B�B�(�B��RB�G�B�  B�z�B���B�p�B�{B��RB�G�B��B�(�B��RB�p�B�  B��\B�
=B��B�{B���B�p�B�  B�z�B���BÅB�(�B���B�\)B�B�Q�B�
=BǮB�Q�B���B�33B�  Bʣ�B�G�B�B�=qB��HB�p�B�(�B���B�33B�B�z�B��BѮB�(�Bң�B�\)B�  B�z�B�
=B�p�B�{BָRB�\)B��
B�=qB��HBٙ�B�(�Bڣ�B��BۮB�Q�B���Bݙ�B�{B�z�B�33B��
B�Q�B��HBᙚB�(�B��B�33B��B�\B���B�B�=qB���B�33B�B�z�B�
=B�B�  B�\B�\)B��B�ffB��HB�p�B�(�B���B�\)B��
B�z�B��B�B�{B��B�p�B�  B�ffB�
=B�B�Q�B��RB�G�B�  B��\B���B��B�(�B��HB�\)B�B�z�B��B��B�  B��RB�G�B���C (�C z�C �RC �C=qC�\C��C  CQ�C��C��C(�C\)CC
=C33C�C�HC�C\)C��C�C=qC�C�C��CQ�C��CC
=CQ�C��C�C	33C	ffC	��C
  C
Q�C
z�C
�RC{CQ�C�\CC  CQ�C��C�HC(�CQ�C��C��C(�CffC�RC
=CQ�C�\C��C�Cp�C�C�C=qC�\C��C
=CQ�C��C��C(�CffC��C  CG�C��C�
C{Cp�CC
=CG�C�\C�C=qCp�C�RC
=Cp�C�C�C=qC��C��C33Cz�CC(�Cp�C�RC��CQ�C�C�C(�Cp�C�RC{CffC�C�CG�C��C�
C �C p�C C!
=C!G�C!�\C!�HC"=qC"z�C"�RC#  C#Q�C#��C#�C$�C$ffC$�RC%
=C%=qC%z�C%��C&�C&ffC&��C&�HC'=qC'�\C'C(  C(\)C(�C)  C)33C)p�C)��C*�C*ffC*��C+  C+Q�C+��C+�
C,33C,�C,��C-
=C-Q�C-��C-��C.G�C.��C.�C/(�C/p�C/��C0(�C0p�C0�RC1  C1Q�C1�C2
=C2Q�C2�\C2�
C3�C3�C3�
C4{C4ffC4�C5  C5Q�C5�C5�C633C6�C6�HC7=qC7�\C7��C8{C8ffC8C9{C9ffC9��C9�C:33C:�C:�
C;33C;�C;C<
=C<\)C<��C=  C=Q�C=�C=�C>(�C>�C>�
C?33C?�C?�
C@{C@\)C@��C@��CA\)CA�CA��CB=qCB�\CB�HCC33CC�CC�CD=qCD�\CD�
CE{CEffCE�RCF{CFffCF�RCG  CGG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�I�A�I�A�K�A�I�A�M�A�S�A�XA�S�A�XA�S�A�VA�VA�XA�XA�ZA�XA�ZA�ZA�ZA�ZA�\)A�^5A�`BA�bNA�E�A���AڸRAک�Aڟ�Aڗ�A��`A�7LA��A��TA�S�A���Aԏ\A��yA�  AС�A�~�A��TA��A�p�A�A�A�\)A�9XAǅA��#A��A��A�`BA�{A�=qA�n�A��^A���A�&�A��uA���A�v�A�33A�-A��#A�v�A�VA��\A��A�ZA��7A�ĜA�v�A���A�jA���A�9XA�K�A��9A�5?A���A��#A�ffA�bNA�M�A��uA��A��hA�-A���A�ȴA�/A�ƨA�5?A���A�;dA� �A�
=A�bNA�S�A�JA���A�bA��A��A��-A�
=A���A��7A���A�(�A�?}A|I�Ax�/Aw�PAu/Ar(�Ap�yApZAo�#Ao��Ao`BAooAl��AidZAgVAd�Ac��Aa��A`��A_%A^E�A]��A\ĜA[�
A["�AZA�AX�AW"�AU�mARĜAO��ALn�AJ�AG;dAC`BAB5?AA`BA>A�A<�uA;A8�A7��A6 �A2�HA2�A1�mA01A-|�A,�DA*�HA'�mA%%A#�PA"�A ��A ~�A I�A�TA/A\)A�A�AbNA�TAS�A�A�FA�A`BA�/A�DAE�Ap�A�AjAt�A+AVA�A�mAE�AE�A��A�A��AoAx�A
z�A	|�A
�+A�A��A�A��A	oA	�AAhsA/AȴA&�AE�Ax�A~�A��A I�@��/@��#A  �A ��A �yA �A ��A $�@�ȴ@�r�@�I�@��9@��@��F@��9@��@���@���@���@��y@�V@�x�@�+@�n�@�M�@��@��h@�%@��9@�Q�@� �@���@��@���@�?}@�Q�@@�hs@�r�@��
@���@�ff@���@��@�@�+@���@��@��@�@���@�n�@�A�@���@���@�C�@�-@�Ĝ@�9X@�t�@��@�ff@�"�@�~�@ڟ�@�x�@�I�@�X@��H@��T@�7L@� �@���@̣�@�  @ʏ\@˕�@��@ȓu@���@�@�~�@���@���@Ɨ�@Ǿw@�O�@�v�@ʇ+@ʸR@ʏ\@��T@�p�@��@�O�@�n�@��-@��@��@�7L@�9X@�1'@��@��`@��@�/@�?}@���@���@��^@���@��T@���@��@�1'@���@�dZ@���@���@���@�(�@�  @���@�33@�M�@�$�@��@��@�@���@���@��!@��@�t�@�S�@�|�@���@�\)@���@�ȴ@���@�v�@���@�I�@��m@���@��P@�t�@�\)@���@���@��+@�V@��-@�Z@��F@��@�K�@�"�@�o@�
=@�@�ȴ@�v�@�ff@�^5@�=q@�@��T@���@�@��h@��@���@�bN@��F@�C�@�;d@�33@�"�@���@��@���@�p�@�`B@�X@�X@�O�@�?}@�7L@��@��`@��D@�1'@�  @��@�t�@�;d@��y@���@���@��j@��u@�9X@��@��w@���@��P@�|�@�t�@�\)@�C�@�"�@�ȴ@�-@��@�Ĝ@�9X@�9X@�9X@��@��F@�K�@�{@���@�x�@�&�@��/@��u@�j@�1'@���@�o@��\@�~�@���@���@�V@���@�V@���@�  @��@�l�@�\)@�+@��@�~�@��@���@���@�Z@�1'@�b@�1@�  @��m@��F@���@�t�@�S�@�;d@�
=@��y@��y@��y@��H@��@���@��+@�E�@��@���@���@�x�@�hs@�G�@���@���@�(�@��
@��w@���@�dZ@�o@��H@���@�n�@�-@��^@�V@���@��@��u@�j@��@��F@��@�K�@���@�~�@�=q@�J@���@�G�@��/@��@���@��@�Z@�Q�@�I�@�9X@� �@���@��
@���@���@��@�t�@�l�@�K�@���@��@���@�x�@��@��9@��9@�z�@� �@��;@���@�K�@�"�@��+@�5?@��@�{@�@���@��@��^@��h@�G�@���@���@��@�9X@�b@���@��;@�ƨ@��F@��@���@���@��P@��@��@�dZ@�"�@��@��@���@�ff@��@��-@��@�/@���@��@�z�@�Z@�9X@�1@��@~ȴ@}@}`B@}�@|�@{�m@{33@z�@y�@x �@w\)@v�R@vE�@u��@t��@t�@t��@s��@s�F@s��@sS�@so@r�!@r�\@r�\@r^5@r�@rJ@q��@q��@q�7@qx�@qx�@qx�@qhs@qX@q&�@q7L@q7L@p��@pr�@pQ�@pbN@pbN@o��@oK�@n�y@n��@n$�@m�T@m�h@m/@l��@l�D@l(�@kdZ@k33@k"�@ko@j�H@j��@i��@ix�@h��@h�9@hbN@g�@f�y@f�y@f�y@fȴ@fv�@fE�@e�-@dZ@c�m@c��@c@b��@bn�@a�7@_�;@_|�@_\)@_+@_
=@^�R@]�T@\�@\��@\Z@[��@[S�@[S�@[S�@[S�@[S�@[S�@[33@Z�@Z~�@Z-@Y�^@X��@X��@X�@XbN@XA�@X1'@W�@W|�@V�y@V@U/@Tj@S�@R=q@Q��@Q�@Q�^@Q�^@Qhs@Q%@PĜ@P�u@PA�@O��@Nȴ@Nv�@NE�@N5?@N@M�-@MO�@M�@M�@L��@L�j@L1@Kƨ@KdZ@KdZ@KS�@J�H@J�!@J�!@JM�@I��@Ihs@IG�@H�u@HbN@HA�@H �@G�@G|�@G\)@G;d@G�@G�@G�@G�@G�@G�@G+@G�@F��@F{@E�h@EO�@Dz�@C"�@B~�@@��@@�9@@�9@@�u@@�u@@r�@@A�@@1'@?�;@?|�@?|�@?|�@?|�@?l�@?\)@?;d@>��@>�R@>V@=p�@=V@<�@<�/@<�@<�@;dZ@;"�@:�@:�!@:J@9�@9�^@9��@9x�@8��@8Ĝ@8bN@8 �@8  @7�w@7�P@7K�@7�@6ȴ@6@5p�@4�j@4�D@49X@41@3��@3�m@3"�@2��@2��@2=q@1��@1��@1��@17L@0�@0A�@0A�@0 �@0b@0  @/�@/�;@/��@/|�@/;d@.ȴ@.{@-��@-O�@-/@-�@,��@,��@,�j@,z�@,j@,9X@+dZ@+@+@*��@*n�@*^5@*^5@*^5@*^5@*^5@*^5@*M�@*=q@*-@)��@)G�@)&�@(�9@(r�@(Q�@(Q�@( �@'�w@'�P@'|�@'K�@'+@'�@&�y@&ȴ@&v�@&{@%�@%�-@%p�@%V@$�/@$�/@$�/@$�/@$��@$��@$I�@#�
@#��@#��@#��@#��@#�@#t�@#dZ@#C�@#"�@"��@"�\@"^5@"-@!�#@!��@ ��@ bN@�w@�@
=@��@��@�y@��@�y@�@�R@��@��@�+@v�@ff@5?@��@O�@V@�@�@��@dZ@C�@@~�@n�@^5@^5@M�@M�@=q@�@J@��@��@�9@�;@��@l�@\)@\)@\)@K�@;d@
=@�@5?@��@�@p�@O�@/@�/@�/@�/@j@�m@��@�@�@S�@33@33@"�@"�@o@@��@^5@-@��@�#@��@x�@%@�u@1'@��@�A�;dA�9XA�;dA�G�A�I�A�G�A�M�A�K�A�K�A�G�A�K�A�M�A�I�A�G�A�K�A�Q�A�M�A�K�A�K�A�Q�A�XA�XA�ZA�VA�Q�A�S�A�ZA�XA�S�A�VA�XA�S�A�Q�A�VA�XA�S�A�Q�A�S�A�ZA�XA�S�A�S�A�ZA�ZA�XA�ZA�ZA�ZA�XA�XA�ZA�VA�VA�ZA�\)A�VA�XA�ZA�\)A�ZA�VA�ZA�\)A�ZA�VA�\)A�\)A�XA�XA�^5A�^5A�ZA�ZA�^5A�XA�XA�ZA�ZA�ZA�XA�\)A�\)A�XA�ZA�^5A�ZA�VA�ZA�^5A�XA�VA�XA�\)A�ZA�XA�ZA�^5A�ZA�XA�\)A�^5A�\)A�ZA�ZA�^5A�ZA�XA�\)A�^5A�ZA�ZA�^5A�`BA�bNA�\)A�\)A�`BA�^5A�\)A�^5A�bNA�^5A�ZA�^5A�bNA�bNA�`BA�bNA�dZA�dZA�`BA�`BA�dZA�ffA�^5A�bNA�bNA�bNA�bNA�`BA�bNA�dZA�bNA�C�A�E�A�E�A�=qA�5?A��A�+A�&�A�%A��A��`A��#A���A���A�ĜA���Aں^A�ƨAڶFAڮAڬAڮAڰ!Aک�Aک�AڮAک�Aڥ�Aڧ�Aڧ�Aڥ�Aڟ�Aڟ�Aڡ�Aڟ�Aڛ�Aڝ�Aڡ�Aڡ�Aڝ�Aڝ�Aڡ�Aڛ�Aڗ�Aڕ�AړuAډ7A�jA�1A��A��yA��HA���A�ĜA٣�Aى7Aه+A�t�A�M�A�=qA���A��A��mA���A�r�A�-A�A�A�9XA�VA׾wA�r�A�\)A�/A�"�A�oA��Aֺ^A֑hA։7AփA�v�A�ffA�bNA�`BA�VA�E�A�5?A�(�A��A�A���A���AնFAթ�A՛�A՛�AՑhA�dZA�1A��Aԙ�A�p�A�A�A�
=A��;A��TA��TA��HA��;A��`A��A���A���A��A��AӼjAө�A�M�A��mA�ƨA҇+A�G�A��;A�M�A�33A�  AЇ+A�K�A��A��AϾwAϬAϛ�AϏ\Aω7A�v�A�bNA�M�A�33A�"�A��A�JA�A���A�ȴAΛ�A΅A�VA�7LA�VA���A��TA���AͼjAͩ�A͗�A̓A�z�A�t�A�r�A�l�A�VA�E�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�;dA�-A�$�A̼jA�ffA�O�A�"�A˺^A˟�A˃A�1'A���AʓuA��A��HAɕ�A�1A��A��#AǼjAǡ�A�E�A�1'A�&�A��A�VA�  A���A���A��AƩ�A�\)A��AŮA�A�A�%Aĩ�A�&�A���A×�A�+A��A��A�1A��yA��
A°!A�APA�~�A�\)A�/A���A��uA�Q�A�5?A�/A�%A��/A��A��DA�^5A�?}A�7LA�7LA�-A��A�A���A��!A��PA�dZA�;dA�"�A���A��A���A��A���A���A���A���A���A���A���A���A���A���A���A�z�A�l�A�O�A���A���A��9A��!A��A���A���A���A�z�A�hsA�G�A�bA��A�z�A�z�A�z�A�z�A�dZA�9XA���A��9A�r�A���A�I�A�A���A�VA�;dA���A���A�ĜA��uA�S�A�5?A� �A��A�JA���A��A���A�z�A�
=A�x�A�1A���A�ƨA�A��-A���A���A�K�A�ȴA��A�G�A��A���A��yA��#A�ȴA��RA���A���A��\A��+A�v�A�ffA�bNA�M�A�
=A��/A���A���A��wA���A�1A�z�A�M�A�33A�&�A�
=A��TA���A��wA���A��\A�jA�M�A�oA��
A��A���A��\A�33A��A��jA���A��A��A�33A��A�
=A��TA��jA��DA�dZA�+A��HA�^5A���A���A��7A�z�A�r�A�ZA�VA�S�A�S�A�VA�S�A�O�A�I�A�7LA��A�hsA��A���A��RA��PA�{A��yA�ƨA��9A���A���A�r�A�G�A�;dA�"�A�
=A���A��/A���A�ĜA��FA���A��7A��A�|�A�r�A�hsA�=qA�"�A�1'A���A�;dA�bA�A�JA��A��A��A���A�n�A�
=A��;A��jA�M�A��A�ffA�jA�jA�ffA�bNA�bNA�ffA�dZA�bNA�`BA�bNA�bNA�^5A�\)A�XA�VA�K�A�K�A�?}A�7LA�(�A�
=A��^A�jA�(�A���A��HA��^A���A��hA��A�jA�K�A��A���A���A���A�dZA�S�A�E�A�9XA��-A�  A��A��A���A���A�l�A�G�A�%A��A��A��A�ZA�5?A�1A��;A�ȴA�A��RA���A��A�XA�1'A�&�A�$�A�"�A�{A�A��A��HA���A���A��uA��A�O�A��A���A�VA��hA�1'A���A��;A���A��A���A��+A�r�A�dZA�dZA�`BA�O�A�&�A�oA��TA��A���A�C�A���A��A�l�A�bNA�M�A� �A��`A���A��9A���A��A�z�A�p�A�O�A�+A�(�A�{A�bA��A�A�|�A�&�A��A��hA�bNA�;dA���A��A�bNA�E�A� �A�A���A��PA�\)A�33A��`A�M�A���A���A���A�`BA�E�A�$�A�$�A��A�VA���A��A��A��mA��;A���A��jA���A�`BA�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        A�A�A�I�A�I�A�K�A�I�A�M�A�S�A�XA�S�A�XA�S�A�VA�VA�XA�XA�ZA�XA�ZA�ZA�ZA�ZA�\)A�^5A�`BA�bNA�E�A���AڸRAک�Aڟ�Aڗ�A��`A�7LA��A��TA�S�A���Aԏ\A��yA�  AС�A�~�A��TA��A�p�A�A�A�\)A�9XAǅA��#A��A��A�`BA�{A�=qA�n�A��^A���A�&�A��uA���A�v�A�33A�-A��#A�v�A�VA��\A��A�ZA��7A�ĜA�v�A���A�jA���A�9XA�K�A��9A�5?A���A��#A�ffA�bNA�M�A��uA��A��hA�-A���A�ȴA�/A�ƨA�5?A���A�;dA� �A�
=A�bNA�S�A�JA���A�bA��A��A��-A�
=A���A��7A���A�(�A�?}A|I�Ax�/Aw�PAu/Ar(�Ap�yApZAo�#Ao��Ao`BAooAl��AidZAgVAd�Ac��Aa��A`��A_%A^E�A]��A\ĜA[�
A["�AZA�AX�AW"�AU�mARĜAO��ALn�AJ�AG;dAC`BAB5?AA`BA>A�A<�uA;A8�A7��A6 �A2�HA2�A1�mA01A-|�A,�DA*�HA'�mA%%A#�PA"�A ��A ~�A I�A�TA/A\)A�A�AbNA�TAS�A�A�FA�A`BA�/A�DAE�Ap�A�AjAt�A+AVA�A�mAE�AE�A��A�A��AoAx�A
z�A	|�A
�+A�A��A�A��A	oA	�AAhsA/AȴA&�AE�Ax�A~�A��A I�@��/@��#A  �A ��A �yA �A ��A $�@�ȴ@�r�@�I�@��9@��@��F@��9@��@���@���@���@��y@�V@�x�@�+@�n�@�M�@��@��h@�%@��9@�Q�@� �@���@��@���@�?}@�Q�@@�hs@�r�@��
@���@�ff@���@��@�@�+@���@��@��@�@���@�n�@�A�@���@���@�C�@�-@�Ĝ@�9X@�t�@��@�ff@�"�@�~�@ڟ�@�x�@�I�@�X@��H@��T@�7L@� �@���@̣�@�  @ʏ\@˕�@��@ȓu@���@�@�~�@���@���@Ɨ�@Ǿw@�O�@�v�@ʇ+@ʸR@ʏ\@��T@�p�@��@�O�@�n�@��-@��@��@�7L@�9X@�1'@��@��`@��@�/@�?}@���@���@��^@���@��T@���@��@�1'@���@�dZ@���@���@���@�(�@�  @���@�33@�M�@�$�@��@��@�@���@���@��!@��@�t�@�S�@�|�@���@�\)@���@�ȴ@���@�v�@���@�I�@��m@���@��P@�t�@�\)@���@���@��+@�V@��-@�Z@��F@��@�K�@�"�@�o@�
=@�@�ȴ@�v�@�ff@�^5@�=q@�@��T@���@�@��h@��@���@�bN@��F@�C�@�;d@�33@�"�@���@��@���@�p�@�`B@�X@�X@�O�@�?}@�7L@��@��`@��D@�1'@�  @��@�t�@�;d@��y@���@���@��j@��u@�9X@��@��w@���@��P@�|�@�t�@�\)@�C�@�"�@�ȴ@�-@��@�Ĝ@�9X@�9X@�9X@��@��F@�K�@�{@���@�x�@�&�@��/@��u@�j@�1'@���@�o@��\@�~�@���@���@�V@���@�V@���@�  @��@�l�@�\)@�+@��@�~�@��@���@���@�Z@�1'@�b@�1@�  @��m@��F@���@�t�@�S�@�;d@�
=@��y@��y@��y@��H@��@���@��+@�E�@��@���@���@�x�@�hs@�G�@���@���@�(�@��
@��w@���@�dZ@�o@��H@���@�n�@�-@��^@�V@���@��@��u@�j@��@��F@��@�K�@���@�~�@�=q@�J@���@�G�@��/@��@���@��@�Z@�Q�@�I�@�9X@� �@���@��
@���@���@��@�t�@�l�@�K�@���@��@���@�x�@��@��9@��9@�z�@� �@��;@���@�K�@�"�@��+@�5?@��@�{@�@���@��@��^@��h@�G�@���@���@��@�9X@�b@���@��;@�ƨ@��F@��@���@���@��P@��@��@�dZ@�"�@��@��@���@�ff@��@��-@��@�/@���@��@�z�@�Z@�9X@�1@��@~ȴ@}@}`B@}�@|�@{�m@{33@z�@y�@x �@w\)@v�R@vE�@u��@t��@t�@t��@s��@s�F@s��@sS�@so@r�!@r�\@r�\@r^5@r�@rJ@q��@q��@q�7@qx�@qx�@qx�@qhs@qX@q&�@q7L@q7L@p��@pr�@pQ�@pbN@pbN@o��@oK�@n�y@n��@n$�@m�T@m�h@m/@l��@l�D@l(�@kdZ@k33@k"�@ko@j�H@j��@i��@ix�@h��@h�9@hbN@g�@f�y@f�y@f�y@fȴ@fv�@fE�@e�-@dZ@c�m@c��@c@b��@bn�@a�7@_�;@_|�@_\)@_+@_
=@^�R@]�T@\�@\��@\Z@[��@[S�@[S�@[S�@[S�@[S�@[S�@[33@Z�@Z~�@Z-@Y�^@X��@X��@X�@XbN@XA�@X1'@W�@W|�@V�y@V@U/@Tj@S�@R=q@Q��@Q�@Q�^@Q�^@Qhs@Q%@PĜ@P�u@PA�@O��@Nȴ@Nv�@NE�@N5?@N@M�-@MO�@M�@M�@L��@L�j@L1@Kƨ@KdZ@KdZ@KS�@J�H@J�!@J�!@JM�@I��@Ihs@IG�@H�u@HbN@HA�@H �@G�@G|�@G\)@G;d@G�@G�@G�@G�@G�@G�@G+@G�@F��@F{@E�h@EO�@Dz�@C"�@B~�@@��@@�9@@�9@@�u@@�u@@r�@@A�@@1'@?�;@?|�@?|�@?|�@?|�@?l�@?\)@?;d@>��@>�R@>V@=p�@=V@<�@<�/@<�@<�@;dZ@;"�@:�@:�!@:J@9�@9�^@9��@9x�@8��@8Ĝ@8bN@8 �@8  @7�w@7�P@7K�@7�@6ȴ@6@5p�@4�j@4�D@49X@41@3��@3�m@3"�@2��@2��@2=q@1��@1��@1��@17L@0�@0A�@0A�@0 �@0b@0  @/�@/�;@/��@/|�@/;d@.ȴ@.{@-��@-O�@-/@-�@,��@,��@,�j@,z�@,j@,9X@+dZ@+@+@*��@*n�@*^5@*^5@*^5@*^5@*^5@*^5@*M�@*=q@*-@)��@)G�@)&�@(�9@(r�@(Q�@(Q�@( �@'�w@'�P@'|�@'K�@'+@'�@&�y@&ȴ@&v�@&{@%�@%�-@%p�@%V@$�/@$�/@$�/@$�/@$��@$��@$I�@#�
@#��@#��@#��@#��@#�@#t�@#dZ@#C�@#"�@"��@"�\@"^5@"-@!�#@!��@ ��@ bN@�w@�@
=@��@��@�y@��@�y@�@�R@��@��@�+@v�@ff@5?@��@O�@V@�@�@��@dZ@C�@@~�@n�@^5@^5@M�@M�@=q@�@J@��@��@�9@�;@��@l�@\)@\)@\)@K�@;d@
=@�@5?@��@�@p�@O�@/@�/@�/@�/@j@�m@��@�@�@S�@33@33@"�@"�@o@@��@^5@-@��@�#@��@x�@%@�u@1'@��G�O�A�;dA�9XA�;dA�G�A�I�A�G�A�M�A�K�A�K�A�G�A�K�A�M�A�I�A�G�A�K�A�Q�A�M�A�K�A�K�A�Q�A�XA�XA�ZA�VA�Q�A�S�A�ZA�XA�S�A�VA�XA�S�A�Q�A�VA�XA�S�A�Q�A�S�A�ZA�XA�S�A�S�A�ZA�ZA�XA�ZA�ZA�ZA�XA�XA�ZA�VA�VA�ZA�\)A�VA�XA�ZA�\)A�ZA�VA�ZA�\)A�ZA�VA�\)A�\)A�XA�XA�^5A�^5A�ZA�ZA�^5A�XA�XA�ZA�ZA�ZA�XA�\)A�\)A�XA�ZA�^5A�ZA�VA�ZA�^5A�XA�VA�XA�\)A�ZA�XA�ZA�^5A�ZA�XA�\)A�^5A�\)A�ZA�ZA�^5A�ZA�XA�\)A�^5A�ZA�ZA�^5A�`BA�bNA�\)A�\)A�`BA�^5A�\)A�^5A�bNA�^5A�ZA�^5A�bNA�bNA�`BA�bNA�dZA�dZA�`BA�`BA�dZA�ffA�^5A�bNA�bNA�bNA�bNA�`BA�bNA�dZA�bNA�C�A�E�A�E�A�=qA�5?A��A�+A�&�A�%A��A��`A��#A���A���A�ĜA���Aں^A�ƨAڶFAڮAڬAڮAڰ!Aک�Aک�AڮAک�Aڥ�Aڧ�Aڧ�Aڥ�Aڟ�Aڟ�Aڡ�Aڟ�Aڛ�Aڝ�Aڡ�Aڡ�Aڝ�Aڝ�Aڡ�Aڛ�Aڗ�Aڕ�AړuAډ7A�jA�1A��A��yA��HA���A�ĜA٣�Aى7Aه+A�t�A�M�A�=qA���A��A��mA���A�r�A�-A�A�A�9XA�VA׾wA�r�A�\)A�/A�"�A�oA��Aֺ^A֑hA։7AփA�v�A�ffA�bNA�`BA�VA�E�A�5?A�(�A��A�A���A���AնFAթ�A՛�A՛�AՑhA�dZA�1A��Aԙ�A�p�A�A�A�
=A��;A��TA��TA��HA��;A��`A��A���A���A��A��AӼjAө�A�M�A��mA�ƨA҇+A�G�A��;A�M�A�33A�  AЇ+A�K�A��A��AϾwAϬAϛ�AϏ\Aω7A�v�A�bNA�M�A�33A�"�A��A�JA�A���A�ȴAΛ�A΅A�VA�7LA�VA���A��TA���AͼjAͩ�A͗�A̓A�z�A�t�A�r�A�l�A�VA�E�A�E�A�E�A�C�A�A�A�A�A�C�A�E�A�E�A�;dA�-A�$�A̼jA�ffA�O�A�"�A˺^A˟�A˃A�1'A���AʓuA��A��HAɕ�A�1A��A��#AǼjAǡ�A�E�A�1'A�&�A��A�VA�  A���A���A��AƩ�A�\)A��AŮA�A�A�%Aĩ�A�&�A���A×�A�+A��A��A�1A��yA��
A°!A�APA�~�A�\)A�/A���A��uA�Q�A�5?A�/A�%A��/A��A��DA�^5A�?}A�7LA�7LA�-A��A�A���A��!A��PA�dZA�;dA�"�A���A��A���A��A���A���A���A���A���A���A���A���A���A���A���A�z�A�l�A�O�A���A���A��9A��!A��A���A���A���A�z�A�hsA�G�A�bA��A�z�A�z�A�z�A�z�A�dZA�9XA���A��9A�r�A���A�I�A�A���A�VA�;dA���A���A�ĜA��uA�S�A�5?A� �A��A�JA���A��A���A�z�A�
=A�x�A�1A���A�ƨA�A��-A���A���A�K�A�ȴA��A�G�A��A���A��yA��#A�ȴA��RA���A���A��\A��+A�v�A�ffA�bNA�M�A�
=A��/A���A���A��wA���A�1A�z�A�M�A�33A�&�A�
=A��TA���A��wA���A��\A�jA�M�A�oA��
A��A���A��\A�33A��A��jA���A��A��A�33A��A�
=A��TA��jA��DA�dZA�+A��HA�^5A���A���A��7A�z�A�r�A�ZA�VA�S�A�S�A�VA�S�A�O�A�I�A�7LA��A�hsA��A���A��RA��PA�{A��yA�ƨA��9A���A���A�r�A�G�A�;dA�"�A�
=A���A��/A���A�ĜA��FA���A��7A��A�|�A�r�A�hsA�=qA�"�A�1'A���A�;dA�bA�A�JA��A��A��A���A�n�A�
=A��;A��jA�M�A��A�ffA�jA�jA�ffA�bNA�bNA�ffA�dZA�bNA�`BA�bNA�bNA�^5A�\)A�XA�VA�K�A�K�A�?}A�7LA�(�A�
=A��^A�jA�(�A���A��HA��^A���A��hA��A�jA�K�A��A���A���A���A�dZA�S�A�E�A�9XA��-A�  A��A��A���A���A�l�A�G�A�%A��A��A��A�ZA�5?A�1A��;A�ȴA�A��RA���A��A�XA�1'A�&�A�$�A�"�A�{A�A��A��HA���A���A��uA��A�O�A��A���A�VA��hA�1'A���A��;A���A��A���A��+A�r�A�dZA�dZA�`BA�O�A�&�A�oA��TA��A���A�C�A���A��A�l�A�bNA�M�A� �A��`A���A��9A���A��A�z�A�p�A�O�A�+A�(�A�{A�bA��A�A�|�A�&�A��A��hA�bNA�;dA���A��A�bNA�E�A� �A�A���A��PA�\)A�33A��`A�M�A���A���A���A�`BA�E�A�$�A�$�A��A�VA���A��A��A��mA��;A���A��jA���A�`BA�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
�wB
��B
�wB
�CB
�=B
��B
�wB
��B
��B
�B
�B
��B
�qB
�6B
�B
�B
�CB
��B
�=B
��B
��B
�B
�B
�_B
��B
�kB
�1B
�$B
��B
��B
��B
��B
ȴB
��B
ѷB
�UB
� BPBP}B_�Bk�B� B�1B�xB�	B��B��B��B~B�B �B1�B>�B?�B=�B<BK�BS�Bi�B{B}�B{�B��B��B�MB�GB�B�iBs�Bo B]/BL�BE9BI�B;�B.B&LB#:B�B$B�"B��B��B�xB�)B��BںB��B�_B�:B�~B��B~�By	BsMBbNBW�BVmB1�B�B$BGB
�PB
��B
�WB
�qB
��B
�tB
�zB
�B
�+B
b�B
XB
U�B
K�B
?�B
=B
7�B
5tB
1'B
.�B
(�B
�B
xB	��B	��B	�B	�KB	�B	��B	�QB	��B	�HB	��B	��B	�BB	��B	�B	�FB	��B	��B	t�B	poB	]�B	O�B	N�B	GEB	8RB	5�B	,�B	(�B	$�B	B	�B	PB	�B	�B�.B��B�ZB��B�KB�>B��B��B�BߤB�B�vBޞBޞB�B�dBݘB�WB�QB��B��B�B�TB��B	�B	_B	xB	�B	�B	#:B	#�B	'�B	-CB	4�B	4�B	3�B	3hB	5?B	)�B	&�B	"�B	=�B	\�B	g8B	gmB	S�B	S[B	bB	[WB	X�B	V9B	VmB	S&B	N�B	J�B	E�B	B[B	<B	/OB	<jB	U�B	aB	i�B	kQB	pB	n�B	l�B	g8B	s�B	|�B	}�B	{B	v�B	��B	��B	�xB	��B	�~B	��B	�B	��B	�=B	�CB	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�LB	�B	�'B	�B	��B	��B	�UB	�nB	�B	�XB	�RB	��B	�[B	�UB	�B	��B	��B	��B	ƨB	�XB	�#B	�dB	��B	�BB	�HB	�B	͟B	�pB	ΥB	֡B	��B	ϫB	�)B	��B	��B	�*B	�LB	�qB	��B	�}B	�qB	�aB	��B	��B	�zB	�FB	�?B	�B	�wB	�B	��B	�
B	�B	�NB	��B	��B	�B	�B	�B	�B	��B	�pB	��B	�,B	خB	�B	�/B	��B	�`B	�B	��B	��B	�B	��B	��B	�lB	��B	�PB	��B	��B	�B	��B	��B	�%B	�B	�oB	�iB	�cB	��B	��B	�)B	��B	��B	�B	�lB	�DB	��B
�B
�B
�B
\B
�B
SB
�B
SB
�B
�B
�B
�B
�B
�B
@B
�B
�B
B
�B
SB
_B
�B
�B
B
�B
!�B
#:B
$B
$@B
$tB
&LB
&LB
&LB
&�B
&�B
'�B
($B
($B
(XB
(�B
)�B
)�B
+6B
+�B
+kB
+6B
+B
*eB
,=B
,qB
-�B
.}B
.�B
/B
.�B
.�B
.�B
.�B
.�B
/OB
/OB
/�B
0!B
0UB
0UB
0�B
1'B
4�B
5B
5B
5B
6B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7B
7B
8RB
8�B
:*B
9�B
9�B
9$B
9XB
9�B
:^B
<B
;0B
<�B
=<B
=<B
>B
=�B
=�B
>wB
?�B
@OB
@B
?HB
?HB
?�B
B'B
A�B
B[B
DgB
E9B
EB
EmB
EB
EmB
F?B
F?B
G�B
H�B
H�B
H�B
IB
H�B
H�B
IB
IB
IB
I�B
I�B
I�B
JXB
I�B
J#B
J#B
I�B
J#B
I�B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
J�B
K�B
L�B
LdB
L0B
LdB
L�B
M6B
MB
MjB
MB
NB
N�B
PB
PB
PHB
PB
PB
QB
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S&B
S[B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
U2B
T�B
U2B
U2B
T�B
UgB
W
B
V�B
V�B
W�B
W�B
XyB
Y�B
Z�B
Z�B
[#B
[#B
[WB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
\�B
]dB
]�B
]�B
]�B
^jB
^�B
^jB
_B
^�B
_B
^�B
^�B
^�B
_B
^�B
^�B
_B
_B
^�B
_pB
_pB
_�B
`B
`B
`B
`vB
`�B
`vB
aB
a|B
a|B
bB
a�B
bNB
cTB
b�B
b�B
b�B
c�B
c�B
c�B
d�B
f�B
gB
gmB
gmB
h
B
h
B
g�B
h
B
iyB
iB
iyB
iyB
jKB
jKB
jB
jB
j�B
kB
kQB
kQB
k�B
l"B
k�B
lWB
l"B
l"B
lWB
l�B
l"B
k�B
m�B
m]B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
o5B
o5B
oiB
o�B
o�B
oiB
o�B
p;B
p;B
pB
pB
o�B
p;B
qvB
qAB
q�B
q�B
rGB
s�B
s�B
s�B
s�B
s�B
tTB
s�B
uZB
v`B
v+B
v�B
v�B
v�B
v�B
xB
x�B
y	B
y	B
y	B
y	B
y>B
zDB
z�B
zxB
{JB
{�B
{�B
{B
{B
{�B
{B
{B
{�B
{B
|PB
{�B
|PB
}�B
|�B
}"B
}VB
}"B
}"B
}"B
}�B
~(B
~�B
cB
� B
��B
��B
�AB
��B
��B
�uB
�B
�{B
�{B
�{B
��B
�MB
��B
��B
�B
��B
��B
��B
�B
�SB
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
�fB
�1B
�1B
��B
�fB
��B
��B
�fB
�fB
��B
�fB
�fB
�fB
�1B
�1B
��B
��B
�fB
��B
��B
�	B
��B
��B
��B
�xB
��B
�xB
�B
��B
��B
�JB
��B
��B
�~B
�~B
��B
��B
��B
��B
�PB
��B
��B
�(B
�(B
�(B
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
�oB
��B
�@B
�uB
�uB
��B
��B
��B
��B
�FB
��B
�B
��B
��B
��B
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
�_B
�1B
�1B
�1B
�eB
�eB
��B
�eB
�eB
�eB
��B
��B
�7B
�	B
�qB
��B
��B
��B
�B
�CB
�CB
��B
�xB
�xB
�~B
��B
�~B
�OB
��B
��B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
�!B
�VB
��B
��B
��B
�VB
��B
��B
��B
��B
�'B
�'B
��B
�\B
��B
��B
��B
�bB
��B
��B
��B
��B
�B
��B
��B
��B
�:B
�nB
�B
�@B
�@B
�@B
�@B
�@B
�@B
�tB
�@B
�tB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�LB
��B
�B
�B
�B
�RB
�RB
�B
�B
�B
�B
�B
�RB
�RB
�B
�RB
��B
��B
��B
��B
�$B
��B
��B
�*B
�_B
��B
��B
�0B
�0B
�eB
�eB
��B
�6B
��B
��B
�qB
�qB
�=B
�=B
�=B
�=B
�=B
�qB
�CB
�CB
�B
��B
�B
�B
��B
�CB
�B
�IB
��B
�B
�OB
�OB
�!B
��B
��B
��B
�'B
�'B
�[B
��B
��B
��B
��B
��B
��B
�[B
��B
��B
�!B
��B
��B
��B
�=B
�}B
��B
�wB
��B
��B
�B
�CB
�IB
�CB
�qB
�CB
�IB
��B
�B
�wB
��B
�B
�6B
�B
�IB
�B
�B
�B
�wB
�B
�qB
�IB
�CB
�B
�B
��B
�B
��B
�qB
��B
��B
�kB
�kB
��B
��B
�0B
��B
��B
��B
�0B
�qB
�CB
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�qB
�B
�kB
�B
��B
�CB
�=B
�6B
�B
�CB
��B
�=B
��B
��B
�6B
�B
�}B
��B
�CB
��B
�B
�qB
��B
��B
�B
�kB
��B
�wB
�B
�B
�=B
�wB
�B
�6B
�eB
��B
��B
�6B
�CB
��B
�=B
�6B
��B
�B
��B
��B
�qB
�B
�=B
��B
��B
�CB
�=B
��B
�kB
��B
��B
��B
�qB
��B
��B
��B
��B
��B
�qB
�6B
�kB
��B
�}B
�CB
��B
�}B
�B
��B
�CB
�B
��B
�=B
�CB
��B
��B
��B
�=B
��B
��B
��B
��B
�*B
�0B
�B
��B
��B
��B
�B
�\B
�B
��B
�xB
��B
��B
�qB
�kB
��B
��B
��B
�eB
��B
�	B
��B
��B
��B
�kB
��B
��B
��B
��B
��B
��B
��B
�$B
�$B
��B
��B
��B
��B
�MB
��B
��B
�hB
��B
��B
��B
��B
��B
�MB
�GB
�iB
�GB
��B
��B
~�B
��B
�SB
��B
�DB
��B
��B
�=B
�\B
�GB
|�B
�B
�kB
�4B
��B
��B
�0B
�qB
��B
�RB
��B
�gB
ӏB
�gB
��B
ܒB
خB
��B
��B
��B
��B
��B
֡B
��B
ԕB
ܒB
ΥB
��B
ΥB
��B
�B
�B
��B
��B
��B
��B
��B
�aB
�HB
�OB
��B
�mB
�dB
бB
՛B
�B
�jB
�B
�B
�ZB
�&BB�B+B#B(�B?BJ#B@BI�BXBUgBX�B\]B[�B[�B]�B^�B^jB`BBa�Bd�Bg8BffBe�BffBg�Bj�BrGBq�Bt�B|B}VB~�BcB~�B�iB��B�SB��B�%B��B��B��B��B��B��B�JB�xB��B��B��B��B�	B�7B�B��B�B�_B�=B��B��B�B�1B�:B��B��B�bB�~B�OB��B�'BںB�9B��B��B�>BںB�WB�;B�B�B�`B�B�&B�B��BBB�B�BbB@B�B�B%�BYB{B�B�B�BOB�BB�B"hB'�B 'B0�B$B+6B+6B4B;0B?B@OBB�B@�B=�B<6B<jB=BA�BB�BA B=�B>wB>�B=�B>BB;�B=<B=B=<B>BB=�B<6B<6B=B=�B<�B:�B;0B<�B>�B?BM�BT�BXyBS�BT�BQ�BRTBQ�BS�BU2BR�BW�B`BBv�Bm�Bl�Bk�BjBm�Bs�Bu�BtBs�B��B�"B��B�JBzBv�B}"Bx�By	B�B�4B.B|ByrBx8BxlBzB}�B��B�(B�uB�~B�B}VB}VB~�B~(B��B��B��B�B��B�B�AB��B��B�B�MB��B��B�AB�uB��B��B�iB��B�rB~�BzDBxB~]B�B�MB�oB~�B{Bv`By>By>BtBsBtBp�Bv�BqvBxBlWBzDBx�Bs�BgBk�B`vBe`Bt�Bp�BX�BQ�BQBT�BO�BN�BK)BMjBN�BS�BC-BB�BC�BFtBC�BI�BEBF?BF?BD�BEBD�BEmBH�BM�BU2BE�B9�B9�BB[BJ#B8B=<B3�B/�B/OB0�B4nB,qB*�B)�B(�B+6B$�B&LB%FB*�B"�B!�B!�B"�B!�B~B"�BkBJ�B(�B�B�B�B=B�B�B�B�BB	�B~BYBqB 4B��B��B��B��B��B��B��B�JB��B�B��B��B�DB�B�B�JB�fB�>B�8B��B��B��B�B�fB�	B�cB�B�B�WB��B�B�B�5B�B�yB��B��B�#BخBӏB�2B�)B�BרB��B�?B�^B�B��B�B��B��B�B�B�3B�B�B�FB��B�=B��B�B��B��B�-B�!B�-B��B�\B�4B�IB��B�eB�1B��B��B��B�$B�B�B�lB��B}"B.B~�B~�B{�B}"Bv�BwfByrB|PBv�Bz�BrB|PBu�Bs�Bn/Bd&Ba�Bd�BgBe`Ba�B\)B[�BXyBV�BV�BZ�BYBO�BOBLdBN�BK^BYBb�BW�B7B.IB-CB9XB*�B$tB"�B�B!B$B�BBkB!bB,qB�BVBBB�B�BBoBuBoB
�.B
��B
�PB
��B
��B
�JB
��B
�lB
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021070717303320210707173033IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021071719005020210717190050QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021071719005020210717190050QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365020220126093650IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                