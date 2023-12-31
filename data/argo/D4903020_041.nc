CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-06-16T06:09:55Z creation; 2021-03-26T17:00:57Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20200616060955  20210326170205  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               )   )AA  AOAO7836_008777_041                 7836_008777_041                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�!�O�Y@�!�O�Y11  @�!�Ov_�@�!�Ov_�@<��eU�@<��eU��d@#π��d@#π�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @��R@��R@޸RA   A��A ��A,(�A@  AaG�A�Q�A�  A�  A�\)A��A�  A�  A�  A��B�B�
B(�B (�B(  B0  B8  B?�
BH  BP  BX(�B`(�Bh  Bo�Bw�B�B�B��B�  B��
B�{B�=qB�  B��B��B�  B�(�B��B��
B��B��B��B�{B�  B��B��
B��B��B��
B�  B�  B��B�  B�  B�  B�  B��C   C
=C  C��C  C
  C��C��C
=C
=C  C  C��C��C  C  C   C"  C$  C&  C(  C*  C,
=C.{C0{C2  C3��C6  C8  C:  C<  C=��C@  CB
=CC��CE��CH  CI��CK��CN  CP  CQ��CT  CV
=CW�CY��C\  C^  C_�Ca��Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr  Ct  Cv{Cx
=Cz  C{��C~  C�  C�C�
=C�  C���C���C�  C�C���C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�C�C�  C�C�  C�  C�  C�  C�C�  C���C�  C�C�
=C�  C�C�
=C�C�  C�C�  C�C���C�
=C���C�  C�C�  C�  C���C�
=C�C�C���C�  C���C���C���C���C�  C�C�  C�C�C���C�  C�  C���C�  C���C���C���C�  C�C�C���C�  C�  C���C���C���C�  C�C�C�C���C�  C�
=C���C�  C�C�  C�C�  C�C�  C�C���C�C�C�C���C�  C�  C���C�  C�
=C�
=C�  C���C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C��C���D   D � D �qD��DD� D�qD}qD  D}qD�RD}qD�D� D  D� D�qD� D	  D	��D
�D
��DD��D  Dz�D  D� D  D}qD  D}qD�qD�DD� D�D�D  D��D  D� D�D}qD  D� D�qD}qD  D��D  Dz�D�qD��D�D��D�D}qD  D}qD  D� D  Dz�D   D �D!�D!� D"  D"� D"�qD#}qD$  D$� D%�D%�D&�D&}qD&�qD'}qD'��D(}qD(�qD)}qD)�qD*z�D*��D+}qD,�D,}qD,��D-}qD.  D.��D/  D/� D0  D0��D1  D1}qD1�qD2� D3�D3��D4  D4� D5  D5}qD5��D6� D6�qD7� D8  D8}qD8��D9z�D9�qD:��D;D;�D<  D<��D=�D=� D>  D>}qD>��D?}qD@  D@��DA�DA� DB�DB}qDB�qDC� DD�DD��DE  DE� DF  DF� DG�DG� DG�qDH}qDH�qDI� DJDJ� DJ�qDK� DL�DL}qDL��DM� DN�DN� DN��DO}qDO�qDP� DQ  DQxRDQ��DR��DS  DSxRDS�qDT� DT�qDU}qDVDV��DV�qDWz�DW�qDX�DY  DY}qDY�qDZ� D[�D[� D\  D\� D]  D]��D^�D^��D_�D_z�D_�qD`��Da  Da�DbDb� Dc  Dc��Dd�Dd� De  De� Df  Df�Dg  Dg� Dh�Dh� Di  Di��DjDj��Dj�qDk}qDl�Dl��Dm  Dm� Dm�qDnz�Do  Do��Dp  Dp}qDp�qDq� Dr�Dr��Ds  Ds}qDs��Dt}qDu�Du� Dv  Dvz�Dv��Dw}qDw��Dx}qDy  Dy� Dy�qDz}qDz��D{� D|  D|z�D|�qD}}qD}�qD~� D�D� D��D�@ D��HD�� D���D�AHD�� D�� D��D�@ D�}qD���D�  D�B�D��HD�� D�HD�>�D�}qD���D�HD�@ D�~�D��HD��D�B�D���D��HD�  D�AHD�� D���D�HD�@ D�� D��HD�  D�=qD�}qD�� D�  D�>�D�~�D���D�HD�B�D��HD��qD��qD�@ D�� D�� D��D�@ D�}qD��qD�  D�AHD�� D���D��qD�>�D�� D��qD���D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�� D���D�  D�AHD��HD��HD�  D�=qD�~�D���D�  D�>�D�~�D�� D�HD�AHD���D�D��D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�HD�@ D�~�D���D�  D�=qD�~�D��HD�  D�@ D�� D�� D���D�>�D�� D���D�  D�AHD�� D��HD��D�AHD��HD�� D�  D�B�D�� D�� D��D�AHD�}qD��qD���D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D��HD�� D���D�@ D��HD���D���D�@ D�� D�� D���D�=qD�~�D�� D�HD�AHD��HD�� D�  D�AHD�� D�� D�  D�=qD�}qD���D�  D�AHD�� D�D��D�>�D�� D��HD�  D�>�D�}qD��qD���D�AHD��HD�D�HD�>�D�� D�� D���D�=qD�~�D���D�HD�@ D��HD�� D�HD�AHD�� D�� D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�B�D���D�� D���D�@ D�~�D��qD�  D�@ D�� D�� D�HD�@ D��HD���D���D�@ D�� D��HD���D�AHD���D��HD�  D�=qD�~�D�� D�  D�AHDHD�� D��D�B�DÀ DýqD�  D�B�DĀ D�� D��qD�>�D�~�D�� D�  D�>�Dƀ D��HD�HD�@ DǁHD��HD�  D�>�D�}qDȾ�D�  D�@ Dɀ Dɾ�D�  D�>�Dʀ Dʾ�D��)D�>�Dˀ D�D��D�@ D̀ D�� D���D�@ D̀ D�� D�HD�>�D΀ D�� D�  D�@ D�}qDϽqD��qD�=qDЀ Dо�D���D�@ D�~�DѾ�D���D�@ D҃�D�� D���D�B�DӁHD�� D�  D�@ DԁHD��HD�HD�@ D�}qDսqD�  D�AHDցHD�� D���D�@ D�~�D׾�D���D�@ D؁HD�� D�  D�AHDفHD��HD�  D�@ Dڀ D�� D�HD�>�D�}qD۾�D�HD�B�D܁HD�� D�  D�@ D�~�D�� D��D�AHD�}qD޽qD��qD�>�D߀ D��HD�  D�@ D��HD��HD�  D�B�D� D�� D�HD�>�D�~�D⾸D�  D�@ D� D��HD���D�@ D䂏D侸D���D�@ D�~�D徸D���D�@ D�HD澸D�  D�B�D炏D羸D���D�@ D�HD��HD�  D�>�D�~�D�� D��qD�>�D� D��HD��D�@ D� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D��HD�HD�@ D�~�DD�HD�AHD� DﾸD�  D�C�D��HD�D�  D�AHD� D�D�  D�AHD�HD�D�  D�@ D�HD��HD�  D�AHD�HD���D�  D�@ D�}qD��qD��qD�=qD�~�D�� D�HD�AHD�� D�D��D�AHD��HD���D��qD�>�D��HD�� D�HD�AHD��HD���D���?��?8Q�?L��?u?��
?�Q�?Ǯ?�(�?��@   @��@�R@.{@5@@  @G�@O\)@\(�@h��@xQ�@��\@��@���@�\)@�
=@��R@��
@�=q@���@�33@�
=@�(�@��
@���@У�@�Q�@�p�@��
@�=q@�\)@�33@�Q�@��RAG�Az�A
=A
=qA��A\)A�\AA�A�A{A!G�A#�
A'
=A*=qA-p�A0��A4z�A7
=A9��A=p�A@��ADz�AHQ�AL(�AO\)AS33AW�AZ�HA_\)Ac33Ag�Aj�HAo\)As33Aw
=Az=qA~�RA�G�A�33A�p�A�\)A�G�A�33A��A�
=A���A�33A�p�A�\)A�G�A�33A�p�A��A���A��A��A�
=A���A��\A���A��RA�G�A�33A�A�  A�=qA�z�A��RA���A��HA���A�ffA�Q�A�=qA���A�\)Aљ�AӅA�ffA���A��HA���A�ffA��A��HA�p�A�A�=qA���A�\)A�A��
A�{A�  A�=qA�(�A�ffB (�BG�B=qB\)Bz�B�B33BQ�B	��B
�RB  B�B=qB33B(�BG�B�\B�
BG�B�\B�B��BB�HB  Bp�B�HB (�B!��B"�RB#�
B$��B%�B'\)B(��B*=qB+\)B,z�B-p�B.ffB/�
B1p�B2�\B3�B4��B6{B7\)B8��B9�B;
=B<  B=�B>�\B@(�BA�BB{BC33BD��BF=qBG\)BHQ�BI�BJffBK�
BM�BNffBO�BPz�BQp�BR�HBTQ�BUBV�RBW�
BX��BZ{B[�B\��B]��B^�HB`Q�Bap�Bb=qBc�BeG�Bf=qBg33BhQ�BiBk33BlQ�BmG�Bn=qBo�
BqG�Br{Bs
=Btz�Bu�Bw\)Bx(�By�Bz�\B|(�B}�B}�B�B�z�B�
=B��B�(�B���B��B��B��RB�p�B�  B�ffB�33B��B�ffB��HB��B�z�B���B�\)B�  B���B��B�{B�z�B�33B��B���B��B���B�ffB��B��B�=qB�
=B��B��B��RB��B��B�z�B��B��B�Q�B��HB��B�=qB���B�\)B�{B���B�33B���B�ffB��B���B�  B��RB�p�B�  B�ffB��B��B�=qB���B�B�(�B��RB��B�  B�z�B�\)B�B�Q�B��B���B�{B���B�p�B��
B��\B�33B���B�Q�B�
=B�p�B�{B���B�33B�  B��\B�
=B�B��\B��HB���B�Q�B��RB�p�B�(�B��\B�\)B�  B�ffB��B�  B�ffB��HB�B�=qB���BÙ�B�  Bģ�B�\)B�B�z�B�33BǅB�ffB�
=B�p�B�Q�B���B�\)B�(�B̸RB�G�B�{BΏ\B�33B��B�Q�B�33B�B�Q�B��BӮB�{B�
=Bՙ�B�  B��HB�\)B�  B���B�33B�  Bڣ�B��B��B܏\B���B��
B�Q�B���B�B�=qB���BᙚB�{B���B�p�B�(�B��HB�G�B�(�B�RB�\)B�=qB��B�\)B�(�B�z�B�p�B�  B��B�p�B��B���B�G�B�  B�RB�33B�(�B�\B�\)B��B��RB�\)B��B���B�33B�  B��RB�G�B�=qB��\B�p�B�{B���B��B�{B���B�p�C {C z�C �C(�Cz�CC33Cp�C�HC{C�C�
C�C��C�
C(�C�\C��C=qC�\C�HCG�C�C  C=qC��C	  C	Q�C	�C	��C
p�C
��C�CQ�C�
C
=C�CC33C�C�
C=qC�C��C33C��C�CG�C��C  CQ�C�C�C\)C�
C
=C�C��C33C�\C�HC\)C�\C
=CQ�C�C{CffC�
C
=C�\CC33C�C�HC=qC��C��CG�C�RC�Cp�C�C�Cp�C��C=qCp�C��C=qC��C��C \)C �RC!{C!p�C!��C"33C"�C"��C#=qC#�C#��C$p�C$�C%(�C%p�C%�HC&(�C&��C&�HC'G�C'��C(  C(\)C(�C)(�C)\)C)�HC*(�C*�\C*�C+G�C+��C,
=C,p�C,�C-33C-z�C-�HC.=qC.��C.��C/Q�C/C0
=C0�C0C133C1�C1�C2G�C2�\C3
=C3=qC3�C3�C4G�C4�C4�C5
=C5p�C5�\C5�C6{C6\)C6��C6C7(�C7Q�C7��C7�
C8  C8\)C8�C8��C9�C9=qC9��C9��C9��C:\)C:�\C:C;�C;G�C;�\C;�HC<  C<Q�C<��C<C={C=G�C=�C=�
C=��C>\)C>z�C>��C?  C?(�C?�C?�RC?��C@=qC@\)C@�RC@�CA�CAp�CA�\CA�CB�CBQ�CB��CBCC
=CCffCC�CC��CD  CD33CD�\CD��CE  CE(�CEp�CE�CE�HCF33CFG�CF��CF�
CG�CG\)CG�CG�HCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         ?��@   @@  @�  @��R@��R@޸RA   A��A ��A,(�A@  AaG�A�Q�A�  A�  A�\)A��A�  A�  A�  A��B�B�
B(�B (�B(  B0  B8  B?�
BH  BP  BX(�B`(�Bh  Bo�Bw�B�B�B��B�  B��
B�{B�=qB�  B��B��B�  B�(�B��B��
B��B��B��B�{B�  B��B��
B��B��B��
B�  B�  B��B�  B�  B�  B�  B��C   C
=C  C��C  C
  C��C��C
=C
=C  C  C��C��C  C  C   C"  C$  C&  C(  C*  C,
=C.{C0{C2  C3��C6  C8  C:  C<  C=��C@  CB
=CC��CE��CH  CI��CK��CN  CP  CQ��CT  CV
=CW�CY��C\  C^  C_�Ca��Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr  Ct  Cv{Cx
=Cz  C{��C~  C�  C�C�
=C�  C���C���C�  C�C���C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�C�C�  C�C�  C�  C�  C�  C�C�  C���C�  C�C�
=C�  C�C�
=C�C�  C�C�  C�C���C�
=C���C�  C�C�  C�  C���C�
=C�C�C���C�  C���C���C���C���C�  C�C�  C�C�C���C�  C�  C���C�  C���C���C���C�  C�C�C���C�  C�  C���C���C���C�  C�C�C�C���C�  C�
=C���C�  C�C�  C�C�  C�C�  C�C���C�C�C�C���C�  C�  C���C�  C�
=C�
=C�  C���C�  C�  C�  C�  C�  C�C�  C���C�  C�  C���C��C���D   D � D �qD��DD� D�qD}qD  D}qD�RD}qD�D� D  D� D�qD� D	  D	��D
�D
��DD��D  Dz�D  D� D  D}qD  D}qD�qD�DD� D�D�D  D��D  D� D�D}qD  D� D�qD}qD  D��D  Dz�D�qD��D�D��D�D}qD  D}qD  D� D  Dz�D   D �D!�D!� D"  D"� D"�qD#}qD$  D$� D%�D%�D&�D&}qD&�qD'}qD'��D(}qD(�qD)}qD)�qD*z�D*��D+}qD,�D,}qD,��D-}qD.  D.��D/  D/� D0  D0��D1  D1}qD1�qD2� D3�D3��D4  D4� D5  D5}qD5��D6� D6�qD7� D8  D8}qD8��D9z�D9�qD:��D;D;�D<  D<��D=�D=� D>  D>}qD>��D?}qD@  D@��DA�DA� DB�DB}qDB�qDC� DD�DD��DE  DE� DF  DF� DG�DG� DG�qDH}qDH�qDI� DJDJ� DJ�qDK� DL�DL}qDL��DM� DN�DN� DN��DO}qDO�qDP� DQ  DQxRDQ��DR��DS  DSxRDS�qDT� DT�qDU}qDVDV��DV�qDWz�DW�qDX�DY  DY}qDY�qDZ� D[�D[� D\  D\� D]  D]��D^�D^��D_�D_z�D_�qD`��Da  Da�DbDb� Dc  Dc��Dd�Dd� De  De� Df  Df�Dg  Dg� Dh�Dh� Di  Di��DjDj��Dj�qDk}qDl�Dl��Dm  Dm� Dm�qDnz�Do  Do��Dp  Dp}qDp�qDq� Dr�Dr��Ds  Ds}qDs��Dt}qDu�Du� Dv  Dvz�Dv��Dw}qDw��Dx}qDy  Dy� Dy�qDz}qDz��D{� D|  D|z�D|�qD}}qD}�qD~� D�D� D��D�@ D��HD�� D���D�AHD�� D�� D��D�@ D�}qD���D�  D�B�D��HD�� D�HD�>�D�}qD���D�HD�@ D�~�D��HD��D�B�D���D��HD�  D�AHD�� D���D�HD�@ D�� D��HD�  D�=qD�}qD�� D�  D�>�D�~�D���D�HD�B�D��HD��qD��qD�@ D�� D�� D��D�@ D�}qD��qD�  D�AHD�� D���D��qD�>�D�� D��qD���D�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�� D�� D���D�>�D�� D���D�  D�AHD��HD��HD�  D�=qD�~�D���D�  D�>�D�~�D�� D�HD�AHD���D�D��D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�HD�@ D�~�D���D�  D�=qD�~�D��HD�  D�@ D�� D�� D���D�>�D�� D���D�  D�AHD�� D��HD��D�AHD��HD�� D�  D�B�D�� D�� D��D�AHD�}qD��qD���D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D��HD�� D���D�@ D��HD���D���D�@ D�� D�� D���D�=qD�~�D�� D�HD�AHD��HD�� D�  D�AHD�� D�� D�  D�=qD�}qD���D�  D�AHD�� D�D��D�>�D�� D��HD�  D�>�D�}qD��qD���D�AHD��HD�D�HD�>�D�� D�� D���D�=qD�~�D���D�HD�@ D��HD�� D�HD�AHD�� D�� D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�B�D���D�� D���D�@ D�~�D��qD�  D�@ D�� D�� D�HD�@ D��HD���D���D�@ D�� D��HD���D�AHD���D��HD�  D�=qD�~�D�� D�  D�AHDHD�� D��D�B�DÀ DýqD�  D�B�DĀ D�� D��qD�>�D�~�D�� D�  D�>�Dƀ D��HD�HD�@ DǁHD��HD�  D�>�D�}qDȾ�D�  D�@ Dɀ Dɾ�D�  D�>�Dʀ Dʾ�D��)D�>�Dˀ D�D��D�@ D̀ D�� D���D�@ D̀ D�� D�HD�>�D΀ D�� D�  D�@ D�}qDϽqD��qD�=qDЀ Dо�D���D�@ D�~�DѾ�D���D�@ D҃�D�� D���D�B�DӁHD�� D�  D�@ DԁHD��HD�HD�@ D�}qDսqD�  D�AHDցHD�� D���D�@ D�~�D׾�D���D�@ D؁HD�� D�  D�AHDفHD��HD�  D�@ Dڀ D�� D�HD�>�D�}qD۾�D�HD�B�D܁HD�� D�  D�@ D�~�D�� D��D�AHD�}qD޽qD��qD�>�D߀ D��HD�  D�@ D��HD��HD�  D�B�D� D�� D�HD�>�D�~�D⾸D�  D�@ D� D��HD���D�@ D䂏D侸D���D�@ D�~�D徸D���D�@ D�HD澸D�  D�B�D炏D羸D���D�@ D�HD��HD�  D�>�D�~�D�� D��qD�>�D� D��HD��D�@ D� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D��HD�HD�@ D�~�DD�HD�AHD� DﾸD�  D�C�D��HD�D�  D�AHD� D�D�  D�AHD�HD�D�  D�@ D�HD��HD�  D�AHD�HD���D�  D�@ D�}qD��qD��qD�=qD�~�D�� D�HD�AHD�� D�D��D�AHD��HD���D��qD�>�D��HD�� D�HD�AHD��HD���G�O�?��?8Q�?L��?u?��
?�Q�?Ǯ?�(�?��@   @��@�R@.{@5@@  @G�@O\)@\(�@h��@xQ�@��\@��@���@�\)@�
=@��R@��
@�=q@���@�33@�
=@�(�@��
@���@У�@�Q�@�p�@��
@�=q@�\)@�33@�Q�@��RAG�Az�A
=A
=qA��A\)A�\AA�A�A{A!G�A#�
A'
=A*=qA-p�A0��A4z�A7
=A9��A=p�A@��ADz�AHQ�AL(�AO\)AS33AW�AZ�HA_\)Ac33Ag�Aj�HAo\)As33Aw
=Az=qA~�RA�G�A�33A�p�A�\)A�G�A�33A��A�
=A���A�33A�p�A�\)A�G�A�33A�p�A��A���A��A��A�
=A���A��\A���A��RA�G�A�33A�A�  A�=qA�z�A��RA���A��HA���A�ffA�Q�A�=qA���A�\)Aљ�AӅA�ffA���A��HA���A�ffA��A��HA�p�A�A�=qA���A�\)A�A��
A�{A�  A�=qA�(�A�ffB (�BG�B=qB\)Bz�B�B33BQ�B	��B
�RB  B�B=qB33B(�BG�B�\B�
BG�B�\B�B��BB�HB  Bp�B�HB (�B!��B"�RB#�
B$��B%�B'\)B(��B*=qB+\)B,z�B-p�B.ffB/�
B1p�B2�\B3�B4��B6{B7\)B8��B9�B;
=B<  B=�B>�\B@(�BA�BB{BC33BD��BF=qBG\)BHQ�BI�BJffBK�
BM�BNffBO�BPz�BQp�BR�HBTQ�BUBV�RBW�
BX��BZ{B[�B\��B]��B^�HB`Q�Bap�Bb=qBc�BeG�Bf=qBg33BhQ�BiBk33BlQ�BmG�Bn=qBo�
BqG�Br{Bs
=Btz�Bu�Bw\)Bx(�By�Bz�\B|(�B}�B}�B�B�z�B�
=B��B�(�B���B��B��B��RB�p�B�  B�ffB�33B��B�ffB��HB��B�z�B���B�\)B�  B���B��B�{B�z�B�33B��B���B��B���B�ffB��B��B�=qB�
=B��B��B��RB��B��B�z�B��B��B�Q�B��HB��B�=qB���B�\)B�{B���B�33B���B�ffB��B���B�  B��RB�p�B�  B�ffB��B��B�=qB���B�B�(�B��RB��B�  B�z�B�\)B�B�Q�B��B���B�{B���B�p�B��
B��\B�33B���B�Q�B�
=B�p�B�{B���B�33B�  B��\B�
=B�B��\B��HB���B�Q�B��RB�p�B�(�B��\B�\)B�  B�ffB��B�  B�ffB��HB�B�=qB���BÙ�B�  Bģ�B�\)B�B�z�B�33BǅB�ffB�
=B�p�B�Q�B���B�\)B�(�B̸RB�G�B�{BΏ\B�33B��B�Q�B�33B�B�Q�B��BӮB�{B�
=Bՙ�B�  B��HB�\)B�  B���B�33B�  Bڣ�B��B��B܏\B���B��
B�Q�B���B�B�=qB���BᙚB�{B���B�p�B�(�B��HB�G�B�(�B�RB�\)B�=qB��B�\)B�(�B�z�B�p�B�  B��B�p�B��B���B�G�B�  B�RB�33B�(�B�\B�\)B��B��RB�\)B��B���B�33B�  B��RB�G�B�=qB��\B�p�B�{B���B��B�{B���B�p�C {C z�C �C(�Cz�CC33Cp�C�HC{C�C�
C�C��C�
C(�C�\C��C=qC�\C�HCG�C�C  C=qC��C	  C	Q�C	�C	��C
p�C
��C�CQ�C�
C
=C�CC33C�C�
C=qC�C��C33C��C�CG�C��C  CQ�C�C�C\)C�
C
=C�C��C33C�\C�HC\)C�\C
=CQ�C�C{CffC�
C
=C�\CC33C�C�HC=qC��C��CG�C�RC�Cp�C�C�Cp�C��C=qCp�C��C=qC��C��C \)C �RC!{C!p�C!��C"33C"�C"��C#=qC#�C#��C$p�C$�C%(�C%p�C%�HC&(�C&��C&�HC'G�C'��C(  C(\)C(�C)(�C)\)C)�HC*(�C*�\C*�C+G�C+��C,
=C,p�C,�C-33C-z�C-�HC.=qC.��C.��C/Q�C/C0
=C0�C0C133C1�C1�C2G�C2�\C3
=C3=qC3�C3�C4G�C4�C4�C5
=C5p�C5�\C5�C6{C6\)C6��C6C7(�C7Q�C7��C7�
C8  C8\)C8�C8��C9�C9=qC9��C9��C9��C:\)C:�\C:C;�C;G�C;�\C;�HC<  C<Q�C<��C<C={C=G�C=�C=�
C=��C>\)C>z�C>��C?  C?(�C?�C?�RC?��C@=qC@\)C@�RC@�CA�CAp�CA�\CA�CB�CBQ�CB��CBCC
=CCffCC�CC��CD  CD33CD�\CD��CE  CE(�CEp�CE�CE�HCF33CFG�CF��CF�
CG�CG\)CG�CG�HCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�>@�@��@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΣ�AΝ�A�VA�$�A�%A͇+A���A�O�A�9XA��AɬAŗ�A���A�ĜA��A���A�A�A��!A���A��RA��uA�l�A��uA� �A��A��A�A�~�A�l�A�n�A�C�A�hsA�~�A�  A�ȴA�t�A���A���A��PA��A��wA��PA�E�A��A�A�oA�33A�-A��A�;dA�\)A��A�JA��^A�n�A���A�^5A�n�A���A� �A�33A�O�A�oA�%A�G�A���A��uA�33A���A���A��^A���A�n�A���A��7A�A�A~�RA}�wA{&�Ay�^Ax�Aw��Av��Av�\At��Ar��ApZAn�!Am��Al�Ak&�Ai�PAg�TAfM�Ae��Ae`BAe"�AdbAb��Ab�Aa��Aa�
Aa��Aa�A`�A_l�A^n�A]��A]%A\v�AZ�AY�AX�RAW�PAU�mAU�ASƨAR~�AQp�AP��AP�+APAO+AN��AN1'AM|�AL��AK��AK;dAJ�RAIdZAGt�AF�jAFAE�
AE��AE�AEO�AD~�ABE�A@�HA@^5A?�A>��A=x�A<�uA;7LA;oA:(�A97LA8�A7p�A6�A5��A4$�A2�A1�A1�wA1��A1�hA0M�A.�/A.E�A.5?A. �A-�A-?}A,��A,1A+\)A*�yA*�uA*�A*�\A*�uA*�uA*~�A*A�A)�A)�7A)"�A(�!A(VA'7LA%%A$I�A#��A#p�A"�A ��A �\A bNA -A�
A;dA
=A�A�^A��AVA7LA"�A%AffA��A/A��AjA��AO�A��A�-A�A��A?}AQ�A�wA�A{A\)AȴAv�A��A"�A
�A
1'A	�hAȴAt�AbNA��A�A��AȴAI�A��AO�A�hA �@�dZ@�v�@��@��@��T@���@�?}@��
@�-@��u@�@�V@��/@�@��@�r�@�K�@��@��#@�^@��/@��m@�=q@�`B@�V@�r�@�  @�!@�Q�@��H@�$�@݁@ۍP@ڇ+@��@���@�I�@׮@�S�@֏\@���@�1@�Ĝ@�S�@��@��@Χ�@�ff@̬@˝�@�-@��/@�Z@ǥ�@�5?@Ĵ9@�5?@���@���@���@�\)@�+@��@��!@���@��7@�O�@�j@��#@�`B@�O�@�/@��`@���@�r�@�b@�S�@�%@�bN@�b@��;@�o@��@���@��m@�ƨ@��@�33@��@��h@�dZ@���@�5?@��@���@�/@���@�1@�V@�X@���@�Ĝ@���@���@�Z@��;@�;d@���@�J@��7@�G�@���@�Z@���@���@�33@�o@��@��R@��+@�J@�p�@��u@��F@�l�@�o@���@�{@��#@��^@�x�@���@��m@�E�@��#@��#@���@��-@���@�`B@��@��`@��u@�Z@�b@��@�33@���@���@�ff@�-@�{@��@���@���@���@�`B@�O�@�?}@�7L@�%@���@���@�I�@��;@���@�dZ@�
=@��!@�=q@��#@�X@�j@�;d@��+@�5?@��7@�V@��j@��D@�Q�@��m@��P@��P@��P@�|�@�dZ@�C�@�33@��\@�5?@�{@���@�x�@�?}@��@��9@�z�@�9X@��@���@�K�@�
=@��@��R@�ff@��@���@��7@�p�@�hs@�?}@���@���@��9@��D@�A�@|�@~v�@}�@}V@|��@|�D@|Z@|9X@{�m@{dZ@z�@z��@z��@z��@z��@z�\@z~�@zJ@yX@x��@w�@w|�@v��@vv�@u�T@u�-@up�@u/@u?}@u?}@u?}@u�@t��@t�j@tj@t(�@t1@s�
@s�F@s��@s��@s��@s�@s�@st�@sS�@so@r�H@r�\@rJ@q��@p��@nȴ@m/@l1@k�@ko@j��@j~�@j~�@jn�@j^5@j=q@j=q@j=q@j=q@j=q@j=q@j=q@j-@j-@j-@i�@i�7@i�@h�`@h��@hQ�@hQ�@hb@h  @g�w@g|�@gK�@f�y@fff@f5?@f{@e�T@e�h@d��@d��@dI�@d(�@d�@d1@c��@c"�@b��@b�\@a��@aX@a7L@`��@`�u@`  @_�@_�P@_l�@_\)@_+@^�y@^��@^V@]��@\��@\9X@[��@[S�@["�@["�@["�@[@Z�@Z��@Zn�@Z-@Y�#@Y��@Yx�@X��@X  @W�;@W��@W�w@W��@WK�@W;d@W�@Vff@Up�@U�@T�@Tz�@T1@S33@R�!@RM�@Q��@Q�^@Qx�@P��@Pr�@PA�@Pb@O�@O�w@O��@Ol�@N�@Nv�@NE�@N$�@N@M�@M�-@M/@L�/@L��@Lj@LZ@L9X@K�
@K�F@K@J�\@I��@I��@IX@I&�@H�`@H�u@H�@H �@G�@Gl�@GK�@GK�@G;d@G+@G�@G
=@F�R@FV@E�@E��@E`B@D�@Dj@DZ@D(�@CS�@B��@B~�@B-@A��@A�^@A�7@A7L@A%@@��@@��@@bN@@A�@@1'@@ �@@b@?�;@?��@?��@?�w@?�P@?;d@>�@>V@>{@=�T@=��@=p�@=?}@=�@<j@<�@<�@;�F@:��@:-@9��@9�7@9�@8Ĝ@8bN@7��@7
=@6�+@65?@6{@5�@5��@5@5�h@5p�@5?}@4(�@3��@3��@3��@3��@3S�@2�!@2~�@2=q@2�@2J@2J@1�#@1�^@1�^@1��@1G�@0�`@0Ĝ@0bN@0 �@/�@/�w@/�P@/K�@.�y@.ȴ@.ff@.5?@.{@.@-�@-�@-�@-��@-�h@-`B@-V@,�/@,��@,I�@,(�@+�m@+ƨ@+t�@+33@+o@+o@+@*�!@*-@)�#@)��@)��@)hs@)�@)%@)%@(�`@(Ĝ@(��@(bN@(1'@'�;@'|�@';d@&�y@&�+@&ff@&$�@&{@%�T@%��@%@%@%p�@$�@$�D@$1@#��@#@"��@"n�@"^5@!��@!��@!�^@!��@!�7@!x�@!hs@!X@!&�@!%@ ��@ ��@ �9@ �u@ �@ r�@ bN@ bN@ bN@ Q�@ 1'@ b@�;@l�@;d@�@�@�R@ff@V@E�@5?@$�@{@{@@�-@��@�@�/@��@��@9X@�\@^5@��@�7@G�@&�@%@�@��@��@�@A�@1'@1'@  @��@�w@�w@��@��@�w@��@�P@�P@\)@;d@�@��@V@�@@��@�h@p�@/@�@�j@�D@Z@9X@1@�m@�m@�
@�F@��@�@t�@dZ@@��@�\@�@�@��@�^@hs@G�@G�@&�@��@�@�@r�@bN@bN@bN@bN@Q�@Q�@A�@1'@b@  @��@��@|�@��@�+@v�@v�@v�@v�@V@$�@@�-@�@O�@�@�@��@�D@�D@z�@ƨ@S�@C�@S�@33@
�@
�H@
��@
n�@	��@	��@	��@	�7@	x�@	hs@	hs@	hs@	hs@	X@	&�@	�@��@�`@�9@r�@1'@  @l�@
=@�@��@v�@v�@v�@ff@5?@@�@�@�T@��@@O�@�@V@�@�/@�/@�@�@��@��@��@9X@�@��@ƨ@��@�@C�@33@"�@@��@��@�!@��@�\@��@�\@�\@~�@^5@M�@M�@=q@=q@=q@=qAΟ�AΥ�AΡ�AΡ�AΡ�AΥ�AάAΩ�AΣ�AΣ�AΛ�A�ffA�n�A�n�A�\)A�ZA�=qA�+A�$�A�1'A�/A� �A��A��A�bA�oA�$�A��TAʹ9Aͧ�A͓uA�|�A�p�A�hsA�l�A�l�A�9XA���A̓uA�`BA�\)A�XA�Q�A�M�A�G�A�E�A�A�A�=qA�;dA�1'A�/A�+A��A��A�VA��#A��A�ƨA��A�
=A��;A�?}A�n�A���AĬA�7LAËDA���A�5?A��A���A�r�A���A�S�A���A�\)A���A�jA�bA���A��FA���A���A���A���A���A��\A��+A��A�9XA���A��A�&�A��A���A���A���A��uA��PA�~�A�l�A�XA�+A��A�x�A��A�9XA�ȴA�z�A�?}A��A��#A��uA�bNA�33A�VA�ĜA��PA��A�ffA�/A��A���A��HA���A���A��!A���A���A���A���A���A���A���A��uA��DA��PA��DA��A�p�A�v�A�r�A�`BA�VA��A��PA��jA�p�A�bNA�C�A��A���A�x�A�p�A�hsA�G�A��TA��7A�?}A��TA���A��jA��A��hA�ffA�&�A�ƨA�Q�A�(�A���A�jA�C�A�/A� �A��A���A��#A��
A�A��A���A��A�r�A�^5A�  A��yA���A�x�A�9XA�{A�1A���A��#A�ƨA�ZA�33A�A��
A���A�~�A�^5A�33A��A���A��A��jA���A�v�A�VA�5?A��A���A��/A��RA��A�ZA�5?A��A�bA�
=A�1A�%A���A��A��yA���A��PA�?}A��+A���A�ȴA��wA���A��PA�x�A�XA�A�A�1'A�(�A��A�
=A���A�9XA��A�VA�  A��HA���A�bNA�?}A��A���A���A���A�n�A�Q�A�5?A�{A�A���A��A��HA��#A��A���A�ȴA�A��jA��FA��A���A���A���A��\A��A�v�A�r�A�jA�\)A�I�A�7LA�5?A�1'A�+A�-A�(�A� �A�
=A�
=A�A���A��A���A��wA���A��\A�t�A�M�A��A�A�A���A��/A��!A���A�~�A�+A���A���A���A��A�Q�A�=qA�"�A��A���A��!A���A��uA��A�x�A�t�A�jA�^5A�M�A�A�A�9XA�;dA�33A�
=A��A��^A��DA�Q�A���A��9A�hsA�&�A���A��FA�ffA�9XA�$�A��A�A�
=A��A��A���A��A���A�ƨA���A��FA��A���A��7A�x�A�p�A�jA�jA�dZA�^5A�\)A�Q�A�;dA�$�A��A�1'A���A��DA�r�A�dZA�K�A�oA��
A���A�x�A�`BA�M�A�I�A�(�A�A��`A���A�r�A�dZA�bNA�bNA�Q�A�E�A�+A�A���A��-A�|�A�A�A��A�A���A��yA���A��^A�`BA� �A�  A���A���A��A�I�A��A���A�|�A��A��+A�5?A��mA���A�n�A�VA�A�bNA�JA���A��A��A��;A���A�ƨA���A� �A��jA���A���A��hA��hA��DA�n�A�VA�7LA�+A�$�A� �A��A�A�A���A���A��A��;A��#A���A���A���A�ƨA�ĜA���A���A��jA��9A��A��A��A���A���A���A���A���A���A��DA�p�A�1'A�1A��
A�jA��jA��+A�z�A�S�A��A���A�p�A�C�A�$�A���A��jA�-A�  A�
A��A\)A~��A~�DA~v�A~bNA~M�A~-A~1A}��A}��A}l�A}�A|�!A{7LAz��Az��Az-Az �Az�Ay�
Ay��Ay`BAyC�AyG�Ay?}Ay"�Ax�HAx^5Ax�AxJAx  Aw��Aw�Aw��Aw�^Av�yAvĜAv�RAv��AvȴAv��AvȴAv��Av�\Av�AuG�AuoAt��At��As�As��As��AsG�Ar�ArM�Aq��AqhsAp��Ap�ApJAoAot�Ao+An�/An�uAnz�AnQ�An(�AnAm�TAmAm��Amt�Am+Am
=Al�HAl��Al�Aln�Al$�Akp�Aj�\AjE�Aj9XAj{AiAi�7Ai`BAiAh��AhJAg�;Ag�wAg�AgK�Af��Af�Af �Af  Ae�Ae��Ae�Ae��Ae�Aet�Aep�AehsAedZAedZAe\)AeXAeS�AeK�AeG�AeC�Ae;dAe7LAeoAd�Ad�/Ad��Ad�jAd��Adr�Ac�mAc�Acp�AcdZAc`BAcO�Ac&�AcAb�/Ab��AbĜAb��Abz�AbQ�Ab(�Ab�Ab1Ab  Ab  Aa��Aa��Ab  Aa��Aa��Aa��Aa��Aa�Aa�Aa�Aa�TAa�TAa�;Aa�
Aa�
Aa��Aa��Aa��AaAa�wAa�FAa�-Aa�Aa��Aa�AadZAa;dAa?}Aa;dAa7LAa�Aa
=Aa
=A`��A`�A`�HA`��A`ȴA`�A`jA`-A`�A`bA_�A_�A_�-A_t�A_|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         AΣ�AΝ�A�VA�$�A�%A͇+A���A�O�A�9XA��AɬAŗ�A���A�ĜA��A���A�A�A��!A���A��RA��uA�l�A��uA� �A��A��A�A�~�A�l�A�n�A�C�A�hsA�~�A�  A�ȴA�t�A���A���A��PA��A��wA��PA�E�A��A�A�oA�33A�-A��A�;dA�\)A��A�JA��^A�n�A���A�^5A�n�A���A� �A�33A�O�A�oA�%A�G�A���A��uA�33A���A���A��^A���A�n�A���A��7A�A�A~�RA}�wA{&�Ay�^Ax�Aw��Av��Av�\At��Ar��ApZAn�!Am��Al�Ak&�Ai�PAg�TAfM�Ae��Ae`BAe"�AdbAb��Ab�Aa��Aa�
Aa��Aa�A`�A_l�A^n�A]��A]%A\v�AZ�AY�AX�RAW�PAU�mAU�ASƨAR~�AQp�AP��AP�+APAO+AN��AN1'AM|�AL��AK��AK;dAJ�RAIdZAGt�AF�jAFAE�
AE��AE�AEO�AD~�ABE�A@�HA@^5A?�A>��A=x�A<�uA;7LA;oA:(�A97LA8�A7p�A6�A5��A4$�A2�A1�A1�wA1��A1�hA0M�A.�/A.E�A.5?A. �A-�A-?}A,��A,1A+\)A*�yA*�uA*�A*�\A*�uA*�uA*~�A*A�A)�A)�7A)"�A(�!A(VA'7LA%%A$I�A#��A#p�A"�A ��A �\A bNA -A�
A;dA
=A�A�^A��AVA7LA"�A%AffA��A/A��AjA��AO�A��A�-A�A��A?}AQ�A�wA�A{A\)AȴAv�A��A"�A
�A
1'A	�hAȴAt�AbNA��A�A��AȴAI�A��AO�A�hA �@�dZ@�v�@��@��@��T@���@�?}@��
@�-@��u@�@�V@��/@�@��@�r�@�K�@��@��#@�^@��/@��m@�=q@�`B@�V@�r�@�  @�!@�Q�@��H@�$�@݁@ۍP@ڇ+@��@���@�I�@׮@�S�@֏\@���@�1@�Ĝ@�S�@��@��@Χ�@�ff@̬@˝�@�-@��/@�Z@ǥ�@�5?@Ĵ9@�5?@���@���@���@�\)@�+@��@��!@���@��7@�O�@�j@��#@�`B@�O�@�/@��`@���@�r�@�b@�S�@�%@�bN@�b@��;@�o@��@���@��m@�ƨ@��@�33@��@��h@�dZ@���@�5?@��@���@�/@���@�1@�V@�X@���@�Ĝ@���@���@�Z@��;@�;d@���@�J@��7@�G�@���@�Z@���@���@�33@�o@��@��R@��+@�J@�p�@��u@��F@�l�@�o@���@�{@��#@��^@�x�@���@��m@�E�@��#@��#@���@��-@���@�`B@��@��`@��u@�Z@�b@��@�33@���@���@�ff@�-@�{@��@���@���@���@�`B@�O�@�?}@�7L@�%@���@���@�I�@��;@���@�dZ@�
=@��!@�=q@��#@�X@�j@�;d@��+@�5?@��7@�V@��j@��D@�Q�@��m@��P@��P@��P@�|�@�dZ@�C�@�33@��\@�5?@�{@���@�x�@�?}@��@��9@�z�@�9X@��@���@�K�@�
=@��@��R@�ff@��@���@��7@�p�@�hs@�?}@���@���@��9@��D@�A�@|�@~v�@}�@}V@|��@|�D@|Z@|9X@{�m@{dZ@z�@z��@z��@z��@z��@z�\@z~�@zJ@yX@x��@w�@w|�@v��@vv�@u�T@u�-@up�@u/@u?}@u?}@u?}@u�@t��@t�j@tj@t(�@t1@s�
@s�F@s��@s��@s��@s�@s�@st�@sS�@so@r�H@r�\@rJ@q��@p��@nȴ@m/@l1@k�@ko@j��@j~�@j~�@jn�@j^5@j=q@j=q@j=q@j=q@j=q@j=q@j=q@j-@j-@j-@i�@i�7@i�@h�`@h��@hQ�@hQ�@hb@h  @g�w@g|�@gK�@f�y@fff@f5?@f{@e�T@e�h@d��@d��@dI�@d(�@d�@d1@c��@c"�@b��@b�\@a��@aX@a7L@`��@`�u@`  @_�@_�P@_l�@_\)@_+@^�y@^��@^V@]��@\��@\9X@[��@[S�@["�@["�@["�@[@Z�@Z��@Zn�@Z-@Y�#@Y��@Yx�@X��@X  @W�;@W��@W�w@W��@WK�@W;d@W�@Vff@Up�@U�@T�@Tz�@T1@S33@R�!@RM�@Q��@Q�^@Qx�@P��@Pr�@PA�@Pb@O�@O�w@O��@Ol�@N�@Nv�@NE�@N$�@N@M�@M�-@M/@L�/@L��@Lj@LZ@L9X@K�
@K�F@K@J�\@I��@I��@IX@I&�@H�`@H�u@H�@H �@G�@Gl�@GK�@GK�@G;d@G+@G�@G
=@F�R@FV@E�@E��@E`B@D�@Dj@DZ@D(�@CS�@B��@B~�@B-@A��@A�^@A�7@A7L@A%@@��@@��@@bN@@A�@@1'@@ �@@b@?�;@?��@?��@?�w@?�P@?;d@>�@>V@>{@=�T@=��@=p�@=?}@=�@<j@<�@<�@;�F@:��@:-@9��@9�7@9�@8Ĝ@8bN@7��@7
=@6�+@65?@6{@5�@5��@5@5�h@5p�@5?}@4(�@3��@3��@3��@3��@3S�@2�!@2~�@2=q@2�@2J@2J@1�#@1�^@1�^@1��@1G�@0�`@0Ĝ@0bN@0 �@/�@/�w@/�P@/K�@.�y@.ȴ@.ff@.5?@.{@.@-�@-�@-�@-��@-�h@-`B@-V@,�/@,��@,I�@,(�@+�m@+ƨ@+t�@+33@+o@+o@+@*�!@*-@)�#@)��@)��@)hs@)�@)%@)%@(�`@(Ĝ@(��@(bN@(1'@'�;@'|�@';d@&�y@&�+@&ff@&$�@&{@%�T@%��@%@%@%p�@$�@$�D@$1@#��@#@"��@"n�@"^5@!��@!��@!�^@!��@!�7@!x�@!hs@!X@!&�@!%@ ��@ ��@ �9@ �u@ �@ r�@ bN@ bN@ bN@ Q�@ 1'@ b@�;@l�@;d@�@�@�R@ff@V@E�@5?@$�@{@{@@�-@��@�@�/@��@��@9X@�\@^5@��@�7@G�@&�@%@�@��@��@�@A�@1'@1'@  @��@�w@�w@��@��@�w@��@�P@�P@\)@;d@�@��@V@�@@��@�h@p�@/@�@�j@�D@Z@9X@1@�m@�m@�
@�F@��@�@t�@dZ@@��@�\@�@�@��@�^@hs@G�@G�@&�@��@�@�@r�@bN@bN@bN@bN@Q�@Q�@A�@1'@b@  @��@��@|�@��@�+@v�@v�@v�@v�@V@$�@@�-@�@O�@�@�@��@�D@�D@z�@ƨ@S�@C�@S�@33@
�@
�H@
��@
n�@	��@	��@	��@	�7@	x�@	hs@	hs@	hs@	hs@	X@	&�@	�@��@�`@�9@r�@1'@  @l�@
=@�@��@v�@v�@v�@ff@5?@@�@�@�T@��@@O�@�@V@�@�/@�/@�@�@��@��@��@9X@�@��@ƨ@��@�@C�@33@"�@@��@��@�!@��@�\@��@�\@�\@~�@^5@M�@M�@=q@=q@=qG�O�AΟ�AΥ�AΡ�AΡ�AΡ�AΥ�AάAΩ�AΣ�AΣ�AΛ�A�ffA�n�A�n�A�\)A�ZA�=qA�+A�$�A�1'A�/A� �A��A��A�bA�oA�$�A��TAʹ9Aͧ�A͓uA�|�A�p�A�hsA�l�A�l�A�9XA���A̓uA�`BA�\)A�XA�Q�A�M�A�G�A�E�A�A�A�=qA�;dA�1'A�/A�+A��A��A�VA��#A��A�ƨA��A�
=A��;A�?}A�n�A���AĬA�7LAËDA���A�5?A��A���A�r�A���A�S�A���A�\)A���A�jA�bA���A��FA���A���A���A���A���A��\A��+A��A�9XA���A��A�&�A��A���A���A���A��uA��PA�~�A�l�A�XA�+A��A�x�A��A�9XA�ȴA�z�A�?}A��A��#A��uA�bNA�33A�VA�ĜA��PA��A�ffA�/A��A���A��HA���A���A��!A���A���A���A���A���A���A���A��uA��DA��PA��DA��A�p�A�v�A�r�A�`BA�VA��A��PA��jA�p�A�bNA�C�A��A���A�x�A�p�A�hsA�G�A��TA��7A�?}A��TA���A��jA��A��hA�ffA�&�A�ƨA�Q�A�(�A���A�jA�C�A�/A� �A��A���A��#A��
A�A��A���A��A�r�A�^5A�  A��yA���A�x�A�9XA�{A�1A���A��#A�ƨA�ZA�33A�A��
A���A�~�A�^5A�33A��A���A��A��jA���A�v�A�VA�5?A��A���A��/A��RA��A�ZA�5?A��A�bA�
=A�1A�%A���A��A��yA���A��PA�?}A��+A���A�ȴA��wA���A��PA�x�A�XA�A�A�1'A�(�A��A�
=A���A�9XA��A�VA�  A��HA���A�bNA�?}A��A���A���A���A�n�A�Q�A�5?A�{A�A���A��A��HA��#A��A���A�ȴA�A��jA��FA��A���A���A���A��\A��A�v�A�r�A�jA�\)A�I�A�7LA�5?A�1'A�+A�-A�(�A� �A�
=A�
=A�A���A��A���A��wA���A��\A�t�A�M�A��A�A�A���A��/A��!A���A�~�A�+A���A���A���A��A�Q�A�=qA�"�A��A���A��!A���A��uA��A�x�A�t�A�jA�^5A�M�A�A�A�9XA�;dA�33A�
=A��A��^A��DA�Q�A���A��9A�hsA�&�A���A��FA�ffA�9XA�$�A��A�A�
=A��A��A���A��A���A�ƨA���A��FA��A���A��7A�x�A�p�A�jA�jA�dZA�^5A�\)A�Q�A�;dA�$�A��A�1'A���A��DA�r�A�dZA�K�A�oA��
A���A�x�A�`BA�M�A�I�A�(�A�A��`A���A�r�A�dZA�bNA�bNA�Q�A�E�A�+A�A���A��-A�|�A�A�A��A�A���A��yA���A��^A�`BA� �A�  A���A���A��A�I�A��A���A�|�A��A��+A�5?A��mA���A�n�A�VA�A�bNA�JA���A��A��A��;A���A�ƨA���A� �A��jA���A���A��hA��hA��DA�n�A�VA�7LA�+A�$�A� �A��A�A�A���A���A��A��;A��#A���A���A���A�ƨA�ĜA���A���A��jA��9A��A��A��A���A���A���A���A���A���A��DA�p�A�1'A�1A��
A�jA��jA��+A�z�A�S�A��A���A�p�A�C�A�$�A���A��jA�-A�  A�
A��A\)A~��A~�DA~v�A~bNA~M�A~-A~1A}��A}��A}l�A}�A|�!A{7LAz��Az��Az-Az �Az�Ay�
Ay��Ay`BAyC�AyG�Ay?}Ay"�Ax�HAx^5Ax�AxJAx  Aw��Aw�Aw��Aw�^Av�yAvĜAv�RAv��AvȴAv��AvȴAv��Av�\Av�AuG�AuoAt��At��As�As��As��AsG�Ar�ArM�Aq��AqhsAp��Ap�ApJAoAot�Ao+An�/An�uAnz�AnQ�An(�AnAm�TAmAm��Amt�Am+Am
=Al�HAl��Al�Aln�Al$�Akp�Aj�\AjE�Aj9XAj{AiAi�7Ai`BAiAh��AhJAg�;Ag�wAg�AgK�Af��Af�Af �Af  Ae�Ae��Ae�Ae��Ae�Aet�Aep�AehsAedZAedZAe\)AeXAeS�AeK�AeG�AeC�Ae;dAe7LAeoAd�Ad�/Ad��Ad�jAd��Adr�Ac�mAc�Acp�AcdZAc`BAcO�Ac&�AcAb�/Ab��AbĜAb��Abz�AbQ�Ab(�Ab�Ab1Ab  Ab  Aa��Aa��Ab  Aa��Aa��Aa��Aa��Aa�Aa�Aa�Aa�TAa�TAa�;Aa�
Aa�
Aa��Aa��Aa��AaAa�wAa�FAa�-Aa�Aa��Aa�AadZAa;dAa?}Aa;dAa7LAa�Aa
=Aa
=A`��A`�A`�HA`��A`ȴA`�A`jA`-A`�A`bA_�A_�A_�-A_t�A_|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:*B:�B8�B6�B7�B2�B6zB.IB)_B%FB)_B{B��B��B�B�B��B�$B��B��B�_B��B��B�uBx�Bt�Bi�Bg8Bf2BjKBf�Bd�Bb�B\�B`BN<BL0B5?B �B.B�BfB�B�]B��B�5B�B�B�-B�dB�B�B}"B��B�BcB`BOBA�B7LB'�BYBB
�vB
��B
�B
��B
��B
�YB
��B
��B
��B
��B
~�B
kB
[WB
I�B
@�B
7�B
)*B
&�B
IB
=B
�B
�B
�B	�MB	�B	��B	�sB	бB	�B	��B	��B	��B	��B	�~B	�B	�eB	��B	�uB	�:B	�hB	�B	�lB	�SB	��B	~�B	zB	xB	q�B	j�B	h>B	aB	Y�B	R�B	P}B	NpB	IB	F�B	C�B	B�B	@�B	>�B	=�B	;�B	:^B	8�B	6zB	2aB	1�B	.B	-CB	+6B	+B	*0B	)_B	(XB	#�B	�B	B	{B	
rB	YB	�B�(B�TB�B�B�B��B�|BݘBخBӏB�jB�EBǮB�B�zB��B�9B� B�B�HB�B��B�^B��B�?B�hB��B��B��B�'B�!B�UB�B�IB��B�eB��B�B��B�qB��B�B��B�B��B�rB�B��B��B��B�AB��B�B{Bu�Bp�BncBm�Bl�Bk�BjBh
Bh>Be,Bb�B`�B`�B]/BZ�BW?BWsBS[BQ�BQBN<BM6BL0BJ�BHKBGEBGBEBD�B@�B>�B<�B;�B:�B9XB9�B5�B6B6�B1�B/�B/�B-�B-CB-B,B+�B-B+�B+kB.�B$B#nB%�B%�B%zB%�B$B#:B!�B$B!�B#B!bB �B �B�B#B �B�BB�B�B~BB�BIBIBCBCB=B�B 'B~B�BxBB�BOBB!B�B~BOB!B�B$�B"�B#�B#:B#nB#B#B#�B#�B#:B"hB$B)�B)�B)�B)�B*eB*�B*�B+B,�B0�B1'B1�B1[B3�B5tB9XB:�B:�B;dB<B<6BAUBG�BI�BI�BJ�BK^BL�BNBP}BT�BS�BT�BUgBU�BU�BV�BW�B[�B\�B_;B`�BbBc�BffBh�Bi�Bl"Bl�Bm]Bn/BoiBr�Bu�B{B.B�;B��B�+B��B�B�DB�B�(B�uB�B��B��B��B�\B��B��B�:B�tB�LB�RB��B�6B��B��B��B�aB��B�B�B�tB�?B�B��B��B�B��B��B��B��B��B�B��B��B��B��B�KBɺB��BѷB�BܒB��B��B�sB�B�"B�B�vB�B��B��B��B�+B�2B�fB�.B	 iB	oB	MB	_B		B	DB	�B	�B	�B	B	�B	7B	IB	�B	!-B	#�B	&�B	*�B	*�B	,�B	-B	.�B	0UB	1�B	1�B	2�B	4nB	6�B	;dB	?�B	AUB	B'B	B�B	C�B	EmB	G�B	I�B	K�B	K�B	LdB	L�B	M6B	MjB	M�B	PB	S�B	V�B	ZB	[�B	^5B	`vB	c B	c�B	d�B	f2B	e�B	e�B	e�B	ffB	f�B	g�B	iDB	jKB	j�B	kQB	l"B	l"B	l�B	l�B	l�B	l�B	l�B	m�B	n/B	n�B	o�B	q�B	q�B	w2B	�4B	��B	�B	�PB	�\B	�4B	��B	��B	��B	�oB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�@B	�FB	��B	��B	�_B	��B	��B	��B	��B	�B	�B	�B	��B	�\B	�B	�nB	�B	��B	��B	��B	��B	��B	�qB	��B	�qB	�B	�UB	��B	��B	�tB	��B	��B	�$B	�^B	��B	�qB	�BB	�wB	�wB	�HB	�OB	��B	��B	��B	�B	ɺB	˒B	��B	�6B	�6B	�6B	�jB	��B	�B	ϫB	�HB	ѷB	�TB	��B	��B	خB	�B	�KB	�B	��B	چB	ںB	یB	�dB	�B	�HB	��B	�ZB	��B	�B	�yB	�B	�B	��B	�B	��B	��B	�vB	�B	��B	�GB	�B	�B	�B	��B	�+B	�`B	��B	��B	�fB	��B	�rB	�B	�B	�DB	��B	��B	��B	��B	��B
 �B
�B
B
�B
B
B
B
B
�B
+B
_B
_B
�B
�B
�B
�B
�B
	�B

rB
DB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
uB
�B
{B
�B
MB
�B
B
SB
�B
�B
�B
YB
YB
YB
YB
�B
_B
�B
qB
�B
xB
IB
�B
�B
B
�B
 'B
�B
 �B
"�B
$B
$@B
$�B
%�B
&B
'RB
(�B
)�B
+6B
+�B
+�B
+�B
+�B
+�B
,=B
,B
,�B
.�B
.�B
.�B
.�B
.}B
/OB
/�B
0!B
0�B
0�B
1'B
0�B
1[B
1�B
1[B
1�B
2aB
33B
33B
49B
4�B
4�B
5?B
5?B
5�B
6zB
6�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
9�B
:�B
:�B
;0B
;�B
<6B
<jB
<�B
=qB
=�B
=�B
=�B
=�B
>BB
?B
?�B
?�B
?}B
@OB
@�B
@�B
@�B
@�B
@�B
A B
AUB
A�B
B[B
B�B
B�B
C�B
D3B
D3B
E9B
EB
E9B
E�B
E9B
EmB
F�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K)B
K^B
K^B
K^B
K�B
K�B
L0B
LdB
LdB
L�B
L�B
M6B
M6B
M6B
M�B
M�B
M�B
M�B
M�B
N<B
OB
OvB
PB
P}B
P�B
P�B
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
S[B
S&B
S&B
S[B
S�B
W?B
V�B
WsB
XEB
X�B
X�B
X�B
X�B
YKB
YKB
Y�B
ZB
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[#B
[�B
\]B
\�B
]dB
]dB
]�B
]�B
^B
^5B
^�B
^�B
_;B
_pB
_�B
_�B
`B
`B
`BB
`vB
`vB
`�B
`�B
`�B
a|B
a|B
a�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e,B
e�B
e�B
e�B
gB
gmB
gmB
gmB
gmB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
iB
iyB
i�B
jB
i�B
i�B
k�B
k�B
k�B
k�B
k�B
lWB
lWB
lWB
l�B
m�B
n/B
m�B
ncB
n/B
n/B
n/B
ncB
ncB
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
pB
qAB
q�B
rB
r|B
r|B
r�B
r|B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u%B
u%B
t�B
t�B
t�B
u%B
uZB
uZB
v+B
v`B
v`B
v�B
v�B
w2B
wfB
wfB
w�B
w�B
x8B
x8B
x8B
xlB
xlB
xlB
x�B
xlB
x�B
x�B
x�B
y	B
y	B
y>B
y>B
y>B;�B9$B;0B:�B;0B9�B8�B9�B;�B:�B<jB=B7�B7LB5B:�B:*B8�B8�B5?B5�B:^B5?B4B4�B49B6BCaBL�B5�B9�B7�B8�B4nB0�B-�BCaB7�B2�B8�B.IB.IB/B.}B-B,=B,qB+kB*0B*0B'B%FB$B \B�B#�B5�B=�BxB��B�BGBB!B�;B��B�KB,�BޞB�B�HB��B�sB�RB�#B��B�9B�B�0B��B�[B��B�CB�=B�=B�eB�XB�*B��B�)B��B�B��B��B��B�:B��B��B��B��B��B��B�B�FB�qB��B�B��B�~B�7B�@B��B��B��B��B��B�{B�FB�RB�YB��B��B��B��B��B�"B��B��B��B��B��B��B��B��B��B��B��B�YB��B��B�+B�B�rB�lB�hB��B��B�1B��B��B��B�B�B}"B}VB��B�B~(B�B��Bv�Bx8Bv`BtTBu�BtTB��BwfBk�BxlBo5Bm]BlWBjBk�Bj�BgBe�BjKBe�Be`BhsBb�Bc�BncBa�Bd�Bm]Bi�BcTBd�Be�Bh>BgBzxBe�Bm�Bh>Bj�Bd&Bf�Bd�Bd�BffBffBdZBd&Bh
Be�Bf�Ba�Bb�BaHBcTBd�Bb�BbBa|B_�B^�B\�B\]B\�B]dBZ�B\�B\)B`B�BN�BNBP}BQ�BN�BN�BNpBNpBK�BI�BEmBHBL�BQBOBGzB>wB3hB:�B-�B-�B(�B$�B*�B�B 'B�B�B�BFB\B�B�BVBB�B�B�B~BB
=BB	B
rB�B�B�B�B�B%B�B{B �BoBB�]B��B�B �B��B�JB��B�"B�B��B�B�`B�TB�B�MB�B�B�yB��B��B�mB�B�iB�BߤB��B�B�B�WB�yBҽB��BҽB�9B�-B�BB�}B��B�B��B�0B��B�LB��B�0B��B��B��B�qB��B��B�=B��B��B�SB�%Bx8Bw�BsBr�Bx�B|B��B��B�	B��B��B��B��B��B��B�GB�oB� B� B}�B}�B~(Bz�B{�B}�BzDB�%B�oBe�Bc�BbB[#B`BB[WB^jBb�BN�BM6BJ#BF�BL�BF�BJ�BH�B?B7�B8B9XB8�B7�B7�B8�B6B,�B4�B0�B&B!bB�BOB�B�B!�B�B
�B�B�B�B�B
�DB 4B
��B
�`B
��B
��B
�]B
��B
�B
�?B
��B
�B
�6B
�nB
��B
�[B
�-B
��B
�B
��B
��B
��B
��B
�\B
��B
��B
��B
��B
�CB
��B
�_B
�B
��B
��B
��B
�B
��B
�B
��B
�MB
��B
�MB
�B
�{B
�uB
��B
�:B
�hB
��B
�hB
� B
�.B
�VB
�\B
��B
��B
��B
��B
�~B
�~B
�B
��B
��B
��B
��B
��B
oiB
ncB
rB
x�B
zB
kB
c B
a|B
_�B
cTB
iB
S�B
OB
S[B
J�B
R�B
HB
C�B
EmB
B�B
DgB
B'B
B�B
@�B
<jB
?�B
?�B
OB
+�B
0�B
2-B
'�B
&LB
,=B
)�B
/OB
$tB
"�B
#�B
'�B
(XB
+B
�B
B
OB
�B
xB
�B
�B
OB
�B
�B
�B
�B
�B
�B
�B
!�B
'RB
(XB
\B
.B
�B
B

=B
~B
"B
	�B
B	��B
�B	�`B	�2B	�vB	�B	�B	�2B	�B	�B	�B	�TB	�HB	��B	�jB	�)B	یB	�5B	��B	�gB	�QB	֡B	�TB	�HB	�
B	��B	уB	��B	��B	��B	ĜB	��B	�dB	�B	�qB	�$B	��B	��B	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	��B	�OB	��B	��B	�OB	��B	��B	�~B	��B	�B	�IB	��B	��B	�B	�xB	�xB	�	B	��B	�B	�B	��B	�YB	�+B	�$B	��B	�=B	�1B	�	B	�_B	��B	��B	�7B	�_B	��B	�MB	�{B	��B	�@B	�B	�B	��B	�B	��B	��B	�B	�uB	��B	�B	�uB	�oB	��B	�B	��B	�oB	��B	��B	�:B	��B	��B	��B	��B	�4B	��B	��B	�bB	��B	�PB	�~B	�PB	��B	�xB	��B	�	B	��B	��B	�1B	��B	�.B	�B	��B	�SB	��B	��B	�JB	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         B5tB7�B5tB3�B7fB3�B6�B-]B1ABD�BI�B(>B�$B��B�<B��B�iB�B�[B��B�aB��B�TB�SB|Bu�Bg�Bg�Bf�BkQBf2BdtB`\B^�Bb4BNVBQ�B6+B�B�BB3B�HB��B�lB��B�8B�B��B�dB�B��By�B��B}qB��B`�BN"B?�B7fB'mB�BB
�FB
��B
�ZB
��B
�?B
��B
��B
�~B
��B
�(B
�'B
m�B
[WB
G�B
CGB
6`B
&LB
#�B
�B
sB
�B
&B
�B	�B	�B	یB	�?B	�B	��B	�-B	�TB	��B	�KB	�WB	�B	�mB	��B	�<B	�jB	��B	��B	�EB	��B	}VB	{�B	v�B	w�B	oOB	i_B	f�B	`�B	V�B	Q�B	OBB	L0B	E�B	B'B	@ B	@ B	="B	:�B	:^B	8lB	8B	5%B	2�B	1�B	2�B	+B	)�B	&LB	&B	%B	$�B	%�B	%FB	�B	�B	�B	YB	�B	UB��B�B�;B�B�B��B�!B�qB��B�B�)BB�uB�GB�tB��B��B��B��B�xB��B�JB�B��B�AB��B��B�QB�6B��B��B��B��B�B��B�LB��B��B��B��B�B�HB�oB�.B��B��B�MB��B�MB�4B~BB~�B� BzDBtBk�BiyBj0BiDBh>Be�Bd�Bd�Ba|B_pB_B^�B[qBV�BT�BTBPHBO�BN"BJ�BIBIBG�BC�BDBC�BBuBC�B?B<B9rB6�B5ZB5�B5�B2GB6�B6`B-�B+�B+B(�B'�B'�B'�B(�B*�B)�B.}B-wBpB 'B#:B#nB"4B"hB!BB5B \BVB!B�B]BxB	B!�B�B�BB�BCB�B�BQB�B�BEB_B1B�BIB�B�BsBYB1B�BQB�BBB�BxB�B!�BB�B5BOB�BjBpBBOB�B# B%`B$@B$ZB$�B%zB%�B&B'8B+kB,=B,WB,�B-wB0;B2B5tB5�B5�B6zB7LB9�B?�BC�BD�BD�BE�BF�BG�BJ#BN"BQBN�BO�BP.BPbBP�BRTBSuBWsBX+BZ�B[�B]~B_;Ba�Bc�BeBf�BgRBh>Bi*Bj�BncBq�Bw�BzDB|jBHB��B��B��B�YB��B��B��B�KB�B�B�kB�	B��B��B�5B��B�-B�hB�@B��B��B��B��B�CB�IB��B��B��B��B�UB�GB�GB��B��B�hB�B��B�B�B��B�"B��B� B�{B�SB�B�VB��B��BڠB�B�nB�B�B��B�B� B�iB�B�oB��B��B�3B�*B�B�jB�cB	AB	B	%B	�B	�B	
�B	B	�B	B	�B	eB	CB	�B	"4B	%`B	%`B	'B	'�B	)�B	+B	,"B	,WB	-�B	/�B	2B	6�B	:�B	<B	<�B	=VB	>(B	@4B	B�B	D�B	F%B	FtB	F�B	G+B	G�B	G�B	H�B	K)B	OB	Q�B	T�B	V�B	YB	[WB	]�B	^5B	_VB	`�B	_�B	_�B	`\B	`�B	a|B	b�B	c�B	d�B	eFB	e�B	f�B	f�B	f�B	g8B	gB	gmB	g�B	h>B	h�B	iyB	jB	l=B	mwB	s�B	|B	�B	��B	�1B	�#B	��B	��B	�B	�JB	��B	�6B	�6B	�6B	�6B	�6B	�6B	��B	�jB	�6B	��B	�B	��B	�TB	�B	�FB	�2B	�2B	�SB	��B	��B	��B	�B	�=B	��B	��B	��B	��B	�bB	��B	�zB	�LB	��B	�B	�8B	��B	�B	�qB	��B	�oB	�-B	��B	��B	�?B	�LB	��B	��B	��B	�	B	��B	��B	�dB	��B	�.B	�'B	ĜB	�?B	�_B	ǔB	ǔB	ǮB	��B	�KB	ȴB	�=B	��B	�JB	��B	�"B	ѷB	�&B	�uB	��B	��B	ԕB	��B	�MB	֡B	خB	ںB	�B	�~B	�!B	��B	�B	�&B	�,B	�LB	�mB	�B	�B	�kB	�B	�"B	�qB	��B	�CB	��B	�5B	�!B	�B	��B	�B	�[B	�GB	�B	�B	�B	�nB	��B	�ZB	�+B	��B	��B	�xB	�B	�6B	��B	�<B	��B	��B	��B	��B
 B
�B
�B
�B
�B
�B
AB
�B
{B
MB
B
�B
tB
�B
�B
B
�B

�B
xB
~B
B
jB
B
�B
B
vB
�B
HB
�B
�B
�B
B
hB
�B
�B
�B
�B
�B
&B
�B
B
mB

B
�B
+B
+B
B
QB
�B
�B
)B
�B
�B
�B
�B
 �B
 �B
"hB
#�B
$�B
%�B
&B
&B
&B
%�B
&LB
&�B
&�B
(
B
)�B
)DB
)B
)B
)B
*0B
*eB
*�B
+6B
+QB
+�B
+kB
+�B
+�B
+�B
,qB
-B
-�B
-�B
.�B
/B
/OB
/�B
/�B
0�B
0�B
1�B
2aB
2�B
3B
3B
3B
3MB
33B
3�B
4B
49B
5B
5%B
5�B
6B
6�B
6�B
7LB
8B
8B
8B
8B
8RB
9	B
9�B
:DB
:B
:B
:�B
:�B
;B
;0B
;dB
;dB
;�B
;�B
<jB
=B
=VB
=�B
>wB
>�B
>�B
?�B
?}B
?�B
@ B
?�B
@B
A�B
B[B
B�B
C{B
DgB
D�B
EB
D�B
E9B
EmB
ESB
E�B
E�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
G�B
G�B
HB
HKB
H�B
I�B
J	B
J�B
J�B
K)B
KDB
K�B
K�B
LB
K�B
LB
LB
LB
LdB
M�B
M�B
M�B
M�B
M�B
N"B
O�B
Q�B
Q�B
R:B
R�B
S&B
S@B
S&B
S@B
S�B
S�B
TFB
T{B
TFB
TaB
T�B
UB
T�B
T�B
T�B
T�B
U2B
UMB
UMB
UgB
VB
U�B
V�B
V�B
W?B
W�B
W�B
W�B
XEB
X�B
X�B
YKB
YKB
Y�B
Y�B
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[	B
[	B
[WB
\B
\B
\�B
\�B
]/B
]IB
]~B
]�B
]�B
^B
^OB
_B
^�B
^�B
_!B
^�B
_B
_B
_!B
_B
_!B
_VB
_;B
_�B
_�B
`B
`BB
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
bhB
c B
c B
cTB
c�B
dB
d@B
dZB
d@B
d�B
fLB
fB
f2B
e�B
fB
f�B
f�B
gB
g�B
hsB
hsB
hXB
h�B
h�B
hsB
hsB
h�B
h�B
iB
h�B
iDB
i*B
iDB
i�B
j0B
jeB
j�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
m�B
nB
ncB
o B
oB
oiB
o�B
oiB
o5B
o5B
oiB
o�B
o�B
pB
p�B
p�B
p�B
qAB
qAB
q�B
q�B
q�B
rB
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sB
shB
sMB
s�B
s�G�O�B5�B33B5?B4�B5?B3�B2�B3�B5�B4�B6zB7B1�B1[B/B5B49B2�B2�B/OB/�B4nB/OB.B.�B.IB0!B=qBF�B/�B3�B1�B2�B.}B+B'�B=qB1�B,�B2�B(XB(XB)*B(�B'B&LB&�B%zB$@B$@B!-BVBBkB�B�B/�B7�B�B�B��B�VBB1B�KB�
B�ZB&�BخB�#B�XB��BуB�aB�3B��B�HB�B�?B��B�kB��B�RB�LB�LB�tB�hB�:B��B�9B��B�B��B��B��B�IB��B��B��B��B��B�B�B�VB��B��B�B�	B��B�FB�PB��B��B��B��B��B��B�VB�bB�hB��B��B��B��B�B�1B��B��B�B��B��B��B��B�B�B��B�B�iB�B��B�;B.B��B�{B�xB�B��B�ABz�B� B��B{B}"Bw2BwfB~�B�Bx8B~(B~�Bp�BrGBpoBncBo�BncB{�BqvBe�Br|BiDBgmBffBd&Be�Bd�BaB`BdZB`B_pBb�B\�B]�BhsB[�B^�BgmBc�B]dB_B`BbNBaBt�B`Bh
BbNBd�B^5B`�B_B^�B`vB`vB^jB^5BbB_�B`�B[�B\�B[WB]dB_B\�B\)B[�BY�BX�BV�BVmBW
BW�BT�BV�BV9BZB�%BIBH1BJ�BK�BIBIBH�BH�BE�BC�B?�BBABF�BKDBI7BA�B8�B-�B4�B'�B(
B"�BB$�B�BQB�B�B�BpB	�B	�B
#B�BEBB�B�B�B9BgB9B-B�B�B�B B��B B OB�B��B��B��B�0B��B��B��B��B��B�tB��B�LB��B��B�?B��B�}B��B�wB�:B��B�B�$B�B�B��B�B��B��B�	B�7B�FBՁBңB��B��B��B�cB�VB�.B��B��B��B�DB��B�ZB�%B�vB��B�ZB�B��B��B��B��B�#B�gB��B��B}B�OBraBq�BmCBl�Br�BvFBz�B��B�3B��B~�BB�BB�B}qB{�Bz*Bz*Bw�BxBxRBt�BvBw�BtnB�OB��B`'B]�B\CBUMBZkBU�BX�B\�BH�BG_BDMB@�BF�BABEBB�B9>B2B2GB3�B3B2B2B2�B0;B'B.�B*�B BB�BByB�B�B�B�BB
#B�B
��B
�B
�nB
�^B
�B
��B
�B
��B
ևB
��B
�1B
�hB
�%B
�7B
�`B
��B
��B
��B
�WB
�B
�>B
�B
�B
�
B
�#B
��B
�B
��B
��B
��B
�mB
��B
��B
�,B
� B
� B
��B
�HB
� B
�HB
��B
��B
��B
��B
�PB
��B
��B
��B
�~B
��B
�B
��B
�DB
�rB
��B
��B
��B
�+B
��B
��B
��B
��B
�YB
��B
�;B
�;B
�B
��B
i�B
h�B
lWB
sB
tTB
e`B
]dB
[�B
Y�B
]�B
cTB
N<B
IRB
M�B
E9B
M6B
B[B
>BB
?�B
<�B
>�B
<jB
=<B
:�B
6�B
:*B
9�B
IRB
&B
*�B
,qB
!�B
 �B
&�B
#�B
)�B
�B
B
B
!�B
"�B
%FB
+B
_B
�B
$B
�B
B
1B
�B
�B
B
.B
4B
@B
B
�B
CB
!�B
"�B
	�B

rB
�B
SB
�B
�B
fB
B	�cB	�+B	�.B	�B	�vB	�B	��B	��B	�vB	��B	��B	��B	ݘB	یB	�B	خB	�mB	��B	�yB	�B	ϫB	ԕB	��B	̘B	ʌB	�NB	�9B	��B	�BB	�0B	�<B	��B	�0B	��B	�XB	��B	�hB	��B	�B	�B	��B	�*B	�B	��B	�7B	�B	�=B	�CB	��B	�_B	�eB	�+B	��B	��B	��B	��B	��B	��B	��B	��B	�YB	��B	�$B	�7B	�SB	��B	��B	�MB	�B	�YB	�OB	�B	��B	�oB	�hB	�@B	��B	�uB	�MB	��B	�:B	��B	�{B	��B	�:B	��B	��B	�(B	��B	�VB	�VB	�B	�VB	�B	�"B	�PB	��B	��B	�JB	��B	��B	��B	�PB	��B	��B	��B	�B	�~B	�B	�B	�B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	�MB	��B	��B	�uB	��B	�rB	cB	�B	�B	�;B	~(B	��B	zB	~]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<^O�=�=^$=+Ns<�l�<?x<S1 <#�
<n�<>�<#�
<#�
<#�
<4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<PP�<,%�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9999(+/-0), vertically averaged dS =-0.0055(+/-0.0008)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;    OW V1.0: r =0.9999(+/-0), vertically averaged dS =-0.0055(+/-0.0008)                                                                                                                           SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:2/1,y:1/0.5]; max_breaks=0;                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:2/1,y:1/0.5]; max_breaks=0;                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020061606095520200616060955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020061607005320200616070053QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020061607005320200616070053QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020061910342320200619103423IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617004520210326170045IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617004520210326170045IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617004520210326170045IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                